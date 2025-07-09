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
;;    - Similar commands for parens, brackets, braces, paragraphs
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

(defvar davidc--expand-region-history nil
  "Buffer-local history of region expansions.

This is a list of cons cells (START . END) representing previous
selections, with the most recent first. Used by `davidc-region-grow'
and `davidc-region-shrink' to step through selection history.")


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
    (let ((start (progn (backward-paragraph) (point)))
          (end (progn (forward-paragraph) (point))))
      (when (< start end)
        (if (not around)
            (cons start end)
          (cons (progn
                  (goto-char start)
                  (skip-chars-backward " \t\n")
                  (if (bobp) (point) (1+ (point))))
                (progn
                  (goto-char end)
                  (skip-chars-forward " \t\n")
                  (point))))))))

(defun davidc-region--find-delimiter-bounds (open-char close-char &optional around)
  "Find bounds between OPEN-CHAR and CLOSE-CHAR delimiters.

OPEN-CHAR and CLOSE-CHAR are characters forming a delimiter pair.
If AROUND is non-nil, include the delimiters themselves.

Returns a cons cell (START . END) or nil if no enclosing delimiters.

This function uses two strategies:
1. Try Emacs's `backward-up-list' for quick parsing
2. Fall back to manual search with proper nesting depth tracking

The search respects syntax - delimiters in strings or comments are ignored."
  (let ((pos (point)))
    (save-excursion
      ;; First try using backward-up-list (fast path).
      (condition-case nil
          (progn
            (backward-up-list)
            (when (= (char-after) open-char)
              (let ((start (point)))
                (forward-sexp)
                (when (= (char-before) close-char)
                  (if around
                      (cons start (point))
                    (cons (1+ start) (1- (point))))))))
        ;; Fallback to manual search.
        (error
         (goto-char pos)
         (let ((depth 0) (start nil))
           ;; Search backward for unmatched opener.
           (while (and (not start) (not (bobp)))
             (backward-char)
             (unless (or (nth 3 (syntax-ppss)) (nth 4 (syntax-ppss)))
               (cond
                ((= (char-after) close-char) (cl-incf depth))
                ((= (char-after) open-char)
                 (if (> depth 0)
                     (cl-decf depth)
                   (setq start (point)))))))
           ;; Search forward for matching closer.
           (when start
             (goto-char (1+ start))
             (setq depth 1)  ; We're now inside one level
             (while (and (not (eobp)) (> depth 0))
               (unless (or (nth 3 (syntax-ppss)) (nth 4 (syntax-ppss)))
                 (cond
                  ((= (char-after) open-char) (cl-incf depth))
                  ((= (char-after) close-char) (cl-decf depth))))
               (forward-char))
             (when (zerop depth)
               (if around
                   (cons start (point))
                 (cons (1+ start) (1- (point))))))))))))

(defun davidc-region--delimiter-bounds (delimiter-type &optional around)
  "Get bounds for specific DELIMITER-TYPE.

DELIMITER-TYPE is one of the symbols :paren, :bracket, or :brace.
If AROUND is non-nil, include the delimiters.

Returns a cons cell (START . END) or nil if no enclosing delimiters."
  (cl-case delimiter-type
    (:paren (davidc-region--find-delimiter-bounds ?\( ?\) around))
    (:bracket (davidc-region--find-delimiter-bounds ?\[ ?\] around))
    (:brace (davidc-region--find-delimiter-bounds ?\{ ?\} around))))

(defun davidc-region--any-delimiter-bounds (&optional around)
  "Find the nearest enclosing delimiters of any type.

Searches for all delimiter types defined for the current mode
and returns the closest enclosing pair.

If AROUND is non-nil, include the delimiters.

Returns a cons cell (START . END) or nil if no enclosing delimiters."
  (let ((delimiters (davidc-region--get-delimiters))
        best-bounds best-distance)
    (dolist (pair delimiters)
      (let ((bounds (davidc-region--find-delimiter-bounds
                    (car pair) (cdr pair) around)))
        (when bounds
          (let ((distance (- (point) (car bounds))))
            (when (and (>= distance 0)
                     (or (null best-distance) (< distance best-distance)))
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
  (let ((custom-func (davidc-region--get-mode-definition text-object)))
    (cond
     ;; Mode-specific function.
     ((functionp custom-func)
      (funcall custom-func around))
     ;; Built-in text objects.
     ((eq text-object 'word) (davidc-region--word-bounds around))
     ((eq text-object 'string) (davidc-region--string-bounds around))
     ((eq text-object 'paragraph) (davidc-region--paragraph-bounds around))
     ((eq text-object 'paren) (davidc-region--delimiter-bounds :paren around))
     ((eq text-object 'bracket) (davidc-region--delimiter-bounds :bracket around))
     ((eq text-object 'brace) (davidc-region--delimiter-bounds :brace around))
     ((eq text-object 'delimiter) (davidc-region--any-delimiter-bounds around))
     ;; Emacs things-at-point.
     ((memq text-object '(symbol sexp sentence line defun))
      (bounds-of-thing-at-point text-object)))))


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
         ,(format "Select inner %s at point.
This selects only the contents, excluding delimiters or surrounding whitespace.
Similar to Vim's 'i%s' text object." object (substring (symbol-name object) 0 1))
         (interactive)
         (let ((bounds (davidc-region--get-bounds ',object)))
           (when bounds
             (set-mark (car bounds))
             (goto-char (cdr bounds))
             (activate-mark))))
       (defun ,around-name ()
         ,(format "Select around %s at point.
This selects the contents plus delimiters or surrounding whitespace.
Similar to Vim's 'a%s' text object." object (substring (symbol-name object) 0 1))
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


;;; Grow/shrink implementation.
(defun davidc-region--collect-all-bounds ()
  "Collect all possible text object bounds containing point.

Scans for all text object types (both inner and around variants)
that contain the current cursor position.

Returns a list of cons cells (START . END), with duplicates removed.
The list includes all applicable text objects plus the whole buffer."
  (let ((pos (point))
        bounds-list)
    ;; Collect all text objects (inner and around variants).
    (dolist (obj '(word string paren bracket brace delimiter paragraph))
      (dolist (around '(nil t))
        (let ((bounds (davidc-region--get-bounds obj around)))
          (when (and bounds
                    (<= (car bounds) pos)
                    (<= pos (cdr bounds)))
            (push bounds bounds-list)))))
    ;; Add Emacs things.
    (dolist (thing '(symbol sexp sentence line defun))
      (let ((bounds (davidc-region--get-bounds thing)))
        (when (and bounds
                  (<= (car bounds) pos)
                  (<= pos (cdr bounds)))
          (push bounds bounds-list))))
    ;; Add whole buffer.
    (push (cons (point-min) (point-max)) bounds-list)
    ;; Remove duplicates.
    (cl-delete-duplicates bounds-list :test #'equal)))

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
         (candidates (davidc-region--collect-all-bounds))
         ;; Filter for larger bounds.
         (larger (cl-remove-if-not
                 (lambda (b)
                   (and (> (- (cdr b) (car b)) current-size)
                        (<= (car b) (car current))
                        (>= (cdr b) (cdr current))))
                 candidates))
         ;; Find smallest expansion.
         (next (when larger
                (cl-reduce (lambda (a b)
                            (if (< (- (cdr a) (car a))
                                   (- (cdr b) (car b)))
                                a b))
                          larger))))

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
   ((and davidc--expand-region-history (cdr davidc--expand-region-history))
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
                     (re-search-backward "'''\\|\"\"\"" nil t))
              (let ((start (match-beginning 0))
                    (delim (match-string 0)))
                (goto-char (match-end 0))
                (when (search-forward delim nil t)
                  (when (and (>= pos (+ start 3))
                           (<= pos (- (point) 3)))
                    (if around
                        (cons start (point))
                      (cons (+ start 3) (- (point) 3)))))))))))

  (davidc-region-define-text-object 'python-mode 'string
                                   #'davidc-region--python-string-bounds))

(provide 'davidc-region)
