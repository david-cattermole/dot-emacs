;;; -*- lexical-binding: t -*-
;;;
;;; Enhanced region selection with Vim-like text objects
;;;
;; This package provides enhanced region selection tools for Emacs, including:
;;
;; 1. Vim-like text object selection (inner/around word, string, delimiters, etc.)
;; 2. Mode-specific customization for text object definitions.
;; 3. Smart region growing and shrinking that expands through semantic units.
;;
;; BASIC USAGE:
;;
;; The package provides two main ways to select text:
;;
;; 1. Direct selection commands (like Vim):
;;    - `davidc-region-inner-word' - Select word under cursor
;;    - `davidc-region-around-word' - Select word with surrounding whitespace
;;    - `davidc-region-inner-string' - Select string contents
;;    - `davidc-region-around-string' - Select string with quotes
;;    - `davidc-region-inner-paren' - Select paren contents
;;    - `davidc-region-around-paren' - Select contents with parens
;;    - `davidc-region-inner-bracket' - Select brackets contents
;;    - `davidc-region-around-bracket' - Select contents with brackets
;;    - `davidc-region-inner-brace' - Select braces contents
;;    - `davidc-region-around-brace' - Select contents with braces
;;    - `davidc-region-inner-paragraph' - Select paragraphs contents
;;    - `davidc-region-around-paragraph' - Select contents with paragraphs
;;    - `davidc-region-inner-delimiter' - Select (prompt for delimiter) contents
;;    - `davidc-region-around-delimiter' - Select contents with (prompt for delimiter)
;;    - `davidc-region-inner-defun' - Select defuns contents
;;    - `davidc-region-around-defun' - Select contents with defuns
;;
;; 2. Incremental selection:
;;    - `davidc-region-grow' - Expand selection to next larger unit
;;    - `davidc-region-shrink' - Contract selection to previous smaller unit
;;
;; SUGGESTED KEY BINDINGS:
;;
;;   ;; Vim-like bindings
;;   (global-set-key (kbd "M-v i w") 'davidc-region-inner-word)
;;   (global-set-key (kbd "M-v a w") 'davidc-region-around-word)
;;   (global-set-key (kbd "M-v i s") 'davidc-region-inner-string)
;;   (global-set-key (kbd "M-v a s") 'davidc-region-around-string)
;;   ;; etc...
;;
;;   ;; Grow/shrink bindings
;;   (global-set-key (kbd "M-]") 'davidc-region-grow)
;;   (global-set-key (kbd "M-[") 'davidc-region-shrink)
;;
;; CUSTOMIZATION:
;;
;; You can define mode-specific text objects:
;;
;;   ;; Define custom string bounds for Python mode
;;   (davidc-region-define-text-object 'python-mode 'string
;;     (lambda (around)
;;       ;; Custom implementation
;;       ...))
;;
;;   ;; Define custom delimiters for LaTeX mode
;;   (davidc-region-define-delimiters 'latex-mode
;;     '((?\{ . ?\})
;;       (?\[ . ?\])
;;       (?$ . ?$)))  ; Math delimiters
;;
;; IMPLEMENTATION NOTES:
;;
;; The grow/shrink functionality maintains a history of selections, allowing
;; you to step back through previous selections. The history is reset when
;; you start a new selection from a cursor position.
;;
;; Text object detection respects syntax - it won't match delimiters inside
;; strings or comments, and string detection uses Emacs's syntax parsing.

(require 'cl-lib)

;;; Customization variables.
(defvar davidc-region-mode-definitions nil
  "Alist of mode-specific text object definitions.

Each entry has the form (MAJOR-MODE . DEFINITIONS-ALIST), where
DEFINITIONS-ALIST contains entries like:

  (word . BOUNDS-FUNCTION)
  (string . BOUNDS-FUNCTION)
  (delimiters . ((OPEN . CLOSE) ...))

BOUNDS-FUNCTION should be a function that takes one optional
argument AROUND (boolean) and returns a cons cell (START . END)
representing the bounds of the text object, or nil if no valid
object is found at point.

Example:
  ((python-mode . ((string . my-python-string-bounds)
                   (delimiters . ((?\{ . ?\})
                                 (?\[ . ?\]))))))")

(defvar davidc-region-default-delimiters
  '((?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\})
    (?\" . ?\")
    (?\' . ?\')
    (?\` . ?\`)
    (?< . ?>))
  "Default list of delimiter pairs for smart matching.

Each entry is a cons cell (OPEN-CHAR . CLOSE-CHAR) where both
elements are characters. These are used when searching for
matching delimiters in `davidc-region--any-delimiter-bounds'.")
(defvar-local davidc--expand-region-history nil
  "Buffer-local history of region expansions.

This is a list of cons cells (START . END) representing previous
selections, with the most recent first. Used by `davidc-region-grow'
and `davidc-region-shrink' to step through selection history.")

;; Performance optimisation variables.
(defcustom davidc-region-large-region-threshold 10000
  "Skip small object searches in regions larger than this."
  :type 'integer
  :group 'davidc-region)

(defcustom davidc-region-delimiter-search-limit 50000
  "Maximum distance to search for delimiters."
  :type 'integer
  :group 'davidc-region)

;; Cache variables for performance.
(defvar-local davidc-region--bounds-cache nil
  "Cache for computed bounds to avoid recalculation.")

(defvar-local davidc-region--cache-tick nil
  "Buffer modification tick when cache was last updated.")


;;; Cache management
(defun davidc-region--cache-valid-p ()
  "Check if the bounds cache is still valid."
  (and davidc-region--bounds-cache
       davidc-region--cache-tick
       (= davidc-region--cache-tick (buffer-modified-tick))))

(defun davidc-region--get-cached-bounds (key)
  "Get cached bounds for KEY if cache is valid."
  (when (davidc-region--cache-valid-p)
    (alist-get key davidc-region--bounds-cache)))

(defun davidc-region--set-cached-bounds (key bounds)
  "Set cached BOUNDS for KEY."
  (unless (davidc-region--cache-valid-p)
    (setq davidc-region--bounds-cache nil
          davidc-region--cache-tick (buffer-modified-tick)))
  (setf (alist-get key davidc-region--bounds-cache) bounds))


;;; Mode-specific customization.
(defun davidc-region-define-text-object (mode text-object definition)
  "Define a text object for a specific mode.

MODE is the major mode symbol (e.g., 'python-mode).
TEXT-OBJECT is a symbol identifying the object type (e.g., 'string).
DEFINITION is either:
  - A function taking optional AROUND argument and returning (START . END)
  - For 'delimiters text object, a list of (OPEN . CLOSE) pairs

Example:
  (davidc-region-define-text-object 'python-mode 'string
    (lambda (around) ...))"
  (let ((mode-defs (alist-get mode davidc-region-mode-definitions)))
    (setf (alist-get mode davidc-region-mode-definitions)
          (cons (cons text-object definition)
                (assq-delete-all text-object mode-defs)))))

(defun davidc-region-define-delimiters (mode delimiter-pairs)
  "Define delimiter pairs for a specific mode.

MODE is the major mode symbol.
DELIMITER-PAIRS is a list of cons cells (OPEN-CHAR . CLOSE-CHAR).

This is a convenience function that calls `davidc-region-define-text-object'
with the 'delimiters text object.

Example:
  (davidc-region-define-delimiters 'latex-mode
    '((?\{ . ?\}) (?\[ . ?\]) (?$ . ?$)))"
  (davidc-region-define-text-object mode 'delimiters delimiter-pairs))

(defun davidc-region--get-mode-definition (text-object)
  "Get the definition for TEXT-OBJECT in current mode.

Returns the mode-specific definition if one exists, otherwise nil.
Looks up the definition in `davidc-region-mode-definitions' based
on the current `major-mode'."
  (alist-get text-object (alist-get major-mode davidc-region-mode-definitions)))

(defun davidc-region--get-delimiters ()
  "Get delimiter pairs for current mode.

Returns the mode-specific delimiter list if defined, otherwise
returns `davidc-region-default-delimiters'."
  (or (davidc-region--get-mode-definition 'delimiters)
      davidc-region-default-delimiters))


;;; Core bounds finding functions.
(defun davidc-region--word-bounds (&optional around)
  "Get bounds of word at point.

If AROUND is non-nil, include surrounding whitespace.

Returns a cons cell (START . END) or nil if no word at point.
Uses Emacs's `bounds-of-thing-at-point' for word detection."
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (if (not around)
          bounds
        (save-excursion
          (goto-char (car bounds))
          (skip-chars-backward " \t")
          (cons (point)
                (progn
                  (goto-char (cdr bounds))
                  (skip-chars-forward " \t")
                  (point))))))))

(defun davidc-region--string-bounds (&optional around)
  "Get bounds of string at point.

If AROUND is non-nil, include the delimiting quotes.

Returns a cons cell (START . END) or nil if not in a string.
Uses syntax parsing to detect string boundaries accurately."
  (let ((ppss (syntax-ppss)))
    (when (nth 3 ppss)  ; Inside string
      (let ((start (nth 8 ppss)))
        (save-excursion
          (goto-char start)
          (condition-case nil
              (progn
                (forward-sexp)
                (let ((end (point)))
                  (when (> end (1+ start))  ; Valid string
                    (if around
                        (cons start end)
                      (cons (1+ start) (1- end))))))
            (error nil)))))))

(defun davidc-region--paragraph-bounds (&optional around)
  "Get bounds of paragraph at point.

If AROUND is non-nil, include surrounding blank lines.

Returns a cons cell (START . END) or nil if no valid paragraph.
Uses Emacs's paragraph navigation commands."
  (save-excursion
    (let ((orig (point)))
      (backward-paragraph)
      (let ((start (point)))
        (goto-char orig)
        (forward-paragraph)
        (let ((end (point)))
          (when (< start end)
            (if (not around)
                (cons start end)
              (cons (save-excursion
                      (goto-char start)
                      (skip-chars-backward " \t\n")
                      (if (bobp) (point) (1+ (point))))
                    (save-excursion
                      (goto-char end)
                      (skip-chars-forward " \t\n")
                      (point))))))))))

(defun davidc-region--find-delimiter-bounds (open-char close-char &optional around)
  "Find bounds between OPEN-CHAR and CLOSE-CHAR delimiters.

OPEN-CHAR and CLOSE-CHAR are characters forming a delimiter pair.
If AROUND is non-nil, include the delimiters themselves.

Returns a cons cell (START . END) or nil if no enclosing delimiters.

This function uses two strategies:
1. Try Emacs's `backward-up-list' for quick parsing
2. Fall back to manual search with proper nesting depth tracking

The search respects syntax - delimiters in strings or comments are ignored."
  (let* ((cache-key (list 'delimiter open-char close-char around (point)))
         (cached (davidc-region--get-cached-bounds cache-key)))
    (or cached
        (let ((bounds (davidc-region--find-delimiter-bounds-impl open-char close-char around)))
          (davidc-region--set-cached-bounds cache-key bounds)
          bounds))))

(defun davidc-region--find-delimiter-bounds-impl (open-char close-char &optional around)
  "Implementation of delimiter finding."
  (let ((pos (point))
        (limit (min davidc-region-delimiter-search-limit
                   (- (point-max) (point-min)))))
    (save-excursion
      ;; Try fast built-in parsing first.
      (condition-case nil
          (progn
            (backward-up-list)
            (when (and (= (char-after) open-char)
                      (<= (- pos (point)) limit))
              (let ((start (point)))
                (forward-sexp)
                (when (= (char-before) close-char)
                  (if around
                      (cons start (point))
                    (cons (1+ start) (1- (point))))))))
        ;; Manual search.
        (error
         (goto-char pos)
         (let ((search-start (max (point-min) (- pos limit)))
               (depth 0)
               (start nil)
               (ppss (syntax-ppss)))
           ;; Quick check: if we're in string/comment, bail out.
           (unless (or (nth 3 ppss) (nth 4 ppss))
             ;; Backward search with limit.
             (while (and (not start)
                        (> (point) search-start))
               (backward-char)
               ;; Reuse ppss when possible.
               (when (< (point) (nth 0 ppss))
                 (setq ppss (syntax-ppss)))
               (unless (or (nth 3 ppss) (nth 4 ppss))
                 (let ((char (char-after)))
                   (cond
                    ((= char close-char) (cl-incf depth))
                    ((= char open-char)
                     (if (> depth 0)
                         (cl-decf depth)
                       (setq start (point))))))))
             ;; Forward search if we found start.
             (when start
               (goto-char start)
               (condition-case nil
                   (progn
                     (forward-sexp)
                     (when (and (= (char-before) close-char)
                               (<= (- (point) start) limit))
                       (if around
                           (cons start (point))
                         (cons (1+ start) (1- (point))))))
                 (error nil))))))))))

(defun davidc-region--any-delimiter-bounds (&optional around)
  "Find the nearest enclosing delimiters of any type.

Searches for all delimiter types defined for the current mode
and returns the closest enclosing pair.

If AROUND is non-nil, include the delimiters.

Returns a cons cell (START . END) or nil if no enclosing delimiters."
  (let* ((pos (point))
         (delimiters (davidc-region--get-delimiters))
         (limit (min davidc-region-delimiter-search-limit
                    (- (point-max) (point-min))))
         (search-start (max (point-min) (- pos limit)))
         best-bounds best-distance)

    ;; Try each delimiter type, keeping the closest.
    (dolist (pair delimiters)
      (let ((bounds (davidc-region--find-delimiter-bounds
                    (car pair) (cdr pair) around)))
        (when bounds
          (let ((distance (- pos (car bounds))))
            (when (and (>= distance 0)
                      (or (null best-distance)
                          (< distance best-distance)))
              (setq best-bounds bounds
                    best-distance distance))))))
    best-bounds))


;;; Text object bounds dispatcher.
(defun davidc-region--get-bounds (text-object &optional around)
  "Get bounds for TEXT-OBJECT, dispatching to appropriate function.

TEXT-OBJECT is a symbol identifying the object type (e.g., 'word).
If AROUND is non-nil, include surrounding context.

This is the main dispatcher that:
1. Checks for mode-specific definitions
2. Falls back to built-in implementations
3. Handles Emacs thing-at-point objects

Returns a cons cell (START . END) or nil if no valid object found."
  (let* ((cache-key (list text-object around (point)))
         (cached (davidc-region--get-cached-bounds cache-key)))
    (or cached
        (let ((bounds
               (let ((custom-func (davidc-region--get-mode-definition text-object)))
                 (cond
                  ;; Mode-specific function.
                  ((functionp custom-func)
                   (funcall custom-func around))
                  ;; Built-in text objects.
                  ((eq text-object 'word)
                   (davidc-region--word-bounds around))
                  ((eq text-object 'string)
                   (davidc-region--string-bounds around))
                  ((eq text-object 'paragraph)
                   (davidc-region--paragraph-bounds around))
                  ((eq text-object 'paren)
                   (davidc-region--find-delimiter-bounds ?\( ?\) around))
                  ((eq text-object 'bracket)
                   (davidc-region--find-delimiter-bounds ?\[ ?\] around))
                  ((eq text-object 'brace)
                   (davidc-region--find-delimiter-bounds ?\{ ?\} around))
                  ((eq text-object 'delimiter)
                   (davidc-region--any-delimiter-bounds around))
                  ;; Emacs things-at-point.
                  ((memq text-object '(symbol sexp sentence line defun))
                   (bounds-of-thing-at-point text-object))))))
          (davidc-region--set-cached-bounds cache-key bounds)
          bounds))))


;;; Selection commands.
(defmacro davidc-region--define-inner-around-commands (object)
  "Define inner and around selection commands for OBJECT.

This macro generates two interactive functions:
- davidc-region-inner-OBJECT: Select the object contents
- davidc-region-around-OBJECT: Select object with delimiters/whitespace

OBJECT should be a symbol like 'word, 'string, etc."
  (let ((inner-name (intern (format "davidc-region-inner-%s" object)))
        (around-name (intern (format "davidc-region-around-%s" object))))
    `(progn
       (defun ,inner-name ()
         ,(format "Select inner %s." object)
         (interactive)
         (let ((bounds (davidc-region--get-bounds ',object)))
           (when bounds
             (set-mark (car bounds))
             (goto-char (cdr bounds))
             (activate-mark))))
       (defun ,around-name ()
         ,(format "Select around %s." object)
         (interactive)
         (let ((bounds (davidc-region--get-bounds ',object t)))
           (when bounds
             (set-mark (car bounds))
             (goto-char (cdr bounds))
             (activate-mark)))))))


;; Define all selection commands.
(davidc-region--define-inner-around-commands word)
(davidc-region--define-inner-around-commands string)
(davidc-region--define-inner-around-commands paren)
(davidc-region--define-inner-around-commands bracket)
(davidc-region--define-inner-around-commands brace)
(davidc-region--define-inner-around-commands paragraph)
(davidc-region--define-inner-around-commands delimiter)
(davidc-region--define-inner-around-commands defun)


;;; Grow/shrink implementation.
(defun davidc-region--collect-expansions ()
  "Collect expansions with smart ordering and early termination."
  (let* ((pos (point))
         (current-size (if (region-active-p)
                          (- (region-end) (region-beginning))
                        0))
         (bounds-list '())
         (checked-types '()))

    ;; Clear cache if buffer was modified.
    (unless (davidc-region--cache-valid-p)
      (setq davidc-region--bounds-cache nil
            davidc-region--cache-tick (buffer-modified-tick)))

    ;; Phase 1: Quick things-at-point (usually fastest).
    (when (< current-size davidc-region-large-region-threshold)
      (dolist (thing '(word symbol sexp))
        (let ((bounds (bounds-of-thing-at-point thing)))
          (when (and bounds
                    (<= (car bounds) pos)
                    (<= pos (cdr bounds))
                    (> (- (cdr bounds) (car bounds)) current-size))
            (push bounds bounds-list)
            (push thing checked-types)))))

    ;; Phase 2: Larger structures.
    (dolist (thing '(sentence line paragraph defun))
      (let ((bounds (bounds-of-thing-at-point thing)))
        (when (and bounds
                  (<= (car bounds) pos)
                  (<= pos (cdr bounds))
                  (> (- (cdr bounds) (car bounds)) current-size))
          (push bounds bounds-list)
          (push thing checked-types))))

    ;; Phase 3: Text objects (only if needed and region not too large).
    (when (< current-size davidc-region-large-region-threshold)
      ;; Check string first (common and fast).
      (dolist (around '(nil t))
        (let ((bounds (davidc-region--get-bounds 'string around)))
          (when (and bounds
                    (<= (car bounds) pos)
                    (<= pos (cdr bounds))
                    (> (- (cdr bounds) (car bounds)) current-size))
            (push bounds bounds-list))))

      ;; Then delimiters if still needed.
      (when (< (length bounds-list) 5)  ; Arbitrary cutoff.
        (dolist (type '(paren bracket brace))
          (dolist (around '(nil t))
            (let ((bounds (davidc-region--get-bounds type around)))
              (when (and bounds
                        (<= (car bounds) pos)
                        (<= pos (cdr bounds))
                        (> (- (cdr bounds) (car bounds)) current-size))
                (push bounds bounds-list)))))))

    ;; Always add whole buffer as fallback.
    (push (cons (point-min) (point-max)) bounds-list)

    ;; Remove duplicates and sort by size.
    (cl-delete-duplicates
     (sort bounds-list
           (lambda (a b)
             (< (- (cdr a) (car a))
                (- (cdr b) (car b)))))
     :test #'equal)))

(defun davidc-region-grow ()
  "Expand the selected region to the next larger text object.

Starting from the cursor or current selection, this function finds
the next larger enclosing text object and selects it.

The expansion sequence typically follows:
- Symbol/word at point
- String contents -> Full string with quotes
- Inner delimiters -> Delimiters with brackets
- Current sexp -> Parent sexp
- Current line -> Paragraph -> Function -> Buffer

The exact sequence depends on context and mode-specific definitions.

Maintains a history of expansions for use with `davidc-region-shrink'."
  (interactive)
  (unless (local-variable-p 'davidc--expand-region-history)
    (setq-local davidc--expand-region-history nil))

  (let* ((current (if (region-active-p)
                     (cons (region-beginning) (region-end))
                   (cons (point) (point))))
         (current-size (- (cdr current) (car current)))
         (candidates (davidc-region--collect-expansions))
         ;; Find first valid expansion.
         (next (cl-find-if
                (lambda (b)
                  (and (> (- (cdr b) (car b)) current-size)
                       (<= (car b) (car current))
                       (>= (cdr b) (cdr current))))
                candidates)))

    (cond
     (next
      (when (zerop current-size)
        (setq davidc--expand-region-history nil))
      (push next davidc--expand-region-history)
      (set-mark (car next))
      (goto-char (cdr next))
      (activate-mark))
     ((region-active-p)
      (deactivate-mark)
      (message "No further expansion"))
     (t
      (message "No expansion found")))))

(defun davidc-region-shrink ()
  "Shrink the selected region to the previous smaller text object.

This reverses the effect of `davidc-region-grow' by stepping back
through the expansion history. If there's no history, deactivates
the current selection.

Can be called repeatedly to step back through multiple expansions."
  (interactive)
  (cond
   ((not (region-active-p))
    (message "No active region"))
   ((and davidc--expand-region-history
         (cdr davidc--expand-region-history))
    (pop davidc--expand-region-history)
    (let ((prev (car davidc--expand-region-history)))
      (set-mark (car prev))
      (goto-char (cdr prev))
      (activate-mark)))
   (t
    (deactivate-mark)
    (message "No shrink history"))))


;;; Mode-specific customizations.
(with-eval-after-load 'python
  ;; Python triple-quoted strings.
  (defun davidc-region--python-string-bounds (&optional around)
    "Python-specific string bounds including triple quotes.

Handles both regular strings (using default syntax parsing) and
Python's triple-quoted strings (''' or \"\"\").

If AROUND is non-nil, includes the quote characters."
    (or (davidc-region--string-bounds around)
        (let ((pos (point)))
          (save-excursion
            (when (or (looking-at "'''\\|\"\"\"")
                     (re-search-backward "'''\\|\"\"\""
                                       (max (point-min) (- pos 1000)) t))
              (let ((start (match-beginning 0))
                    (delim (match-string 0)))
                (goto-char (match-end 0))
                (when (search-forward delim
                                    (min (point-max) (+ pos 5000)) t)
                  (when (and (>= pos (+ start 3))
                           (<= pos (- (point) 3)))
                    (if around
                        (cons start (point))
                      (cons (+ start 3) (- (point) 3)))))))))))

  (davidc-region-define-text-object 'python-mode 'string
                                   #'davidc-region--python-string-bounds))

(provide 'davidc-region)
