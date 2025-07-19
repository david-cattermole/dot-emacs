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
;;    - `davidc-region-inner-bracket' - Select bracket contents
;;    - `davidc-region-around-bracket' - Select contents with brackets
;;    - `davidc-region-inner-brace' - Select brace contents
;;    - `davidc-region-around-brace' - Select contents with braces
;;    - `davidc-region-inner-paragraph' - Select paragraph contents
;;    - `davidc-region-around-paragraph' - Select paragraph with blank lines
;;    - `davidc-region-inner-delimiter' - Select any delimiter contents
;;    - `davidc-region-around-delimiter' - Select contents with any delimiter
;;    - `davidc-region-inner-defun' - Select function/defun contents
;;    - `davidc-region-around-defun' - Select entire function/defun
;;
;; 2. Incremental selection:
;;    - `davidc-region-grow' - Expand selection to next larger unit
;;    - `davidc-region-shrink' - Contract selection to previous smaller unit
;;
;; SUGGESTED KEY BINDINGS:
;;
;;   ;; Vim-like bindings
;;   (global-set-key (kbd "C-; w") 'davidc-region-inner-word)
;;   (global-set-key (kbd "C-' w") 'davidc-region-around-word)
;;   (global-set-key (kbd "C-; s") 'davidc-region-inner-string)
;;   (global-set-key (kbd "C-' s") 'davidc-region-around-string)
;;   (global-set-key (kbd "C-; p") 'davidc-region-inner-paragraph)
;;   (global-set-key (kbd "C-' p") 'davidc-region-around-paragraph)
;;   (global-set-key (kbd "C-; d") 'davidc-region-inner-delimiter)
;;   (global-set-key (kbd "C-' d") 'davidc-region-around-delimiter)
;;   (global-set-key (kbd "C-; (") 'davidc-region-inner-paren)
;;   (global-set-key (kbd "C-; )") 'davidc-region-inner-paren)
;;   (global-set-key (kbd "C-' (") 'davidc-region-around-paren)
;;   (global-set-key (kbd "C-' )") 'davidc-region-around-paren)
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
;;   ;; Define custom expansion sequence for a mode
;;   (davidc-region-set-mode-sequence 'latex-mode
;;     '((word . nil) (word . t) (math . nil) (math . t) (defun . nil)))
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
(defgroup davidc-region nil
  "Enhanced region selection with text objects."
  :group 'editing
  :prefix "davidc-region-")

(defcustom davidc-region-delimiter-search-limit 50000
  "Maximum distance to search for delimiters.
This prevents excessive searching in very large buffers.
The search will look at most this many characters backward
from point when searching for opening delimiters."
  :type 'integer
  :group 'davidc-region)

(defcustom davidc-region-default-delimiters
  '((?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\})
    (?\" . ?\")
    (?\' . ?\')
    (?\` . ?\`)
    (?< . ?>))
  "Default delimiter pairs for text object matching.
Each element is a cons cell (OPEN-CHAR . CLOSE-CHAR) where both
elements are characters. These are used when searching for
matching delimiters in `davidc-region--any-delimiter-bounds'.

You can customize this list to add more delimiter types, or
define mode-specific delimiters using `davidc-region-define-delimiters'."
  :type '(alist :key-type character :value-type character)
  :group 'davidc-region)


;;; Mode-specific definitions
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

;; Mode-specific expansion sequences for different major modes.
;;
;; Each sequence defines the order in which text objects are tried
;; when expanding the region with `davidc-region-grow'.
(defvar davidc-region-mode-expansion-sequences
  '((emacs-lisp-mode . ((symbol . nil)
                        (symbol . t)
                        (string . nil)
                        (string . t)
                        (sexp . nil)
                        (sexp . t)
                        (defun . nil)))

    (python-mode . ((word . nil)
                    (word . t)
                    (string . nil)
                    (string . t)
                    (paren . nil)
                    (paren . t)
                    (bracket . nil)
                    (bracket . t)
                    (defun . nil)))

    (c-mode . ((word . nil)
               (word . t)
               (string . nil)
               (string . t)
               (paren . nil)
               (paren . t)
               (brace . nil)
               (brace . t)
               (defun . nil)))

    (c++-mode . ((word . nil)
                 (word . t)
                 (string . nil)
                 (string . t)
                 (paren . nil)
                 (paren . t)
                 (brace . nil)
                 (brace . t)
                 (defun . nil)))

    ;; Rust - similar to C++ but with emphasis on different brackets.
    (rust-mode . ((word . nil)
                  (word . t)
                  (string . nil)
                  (string . t)
                  (paren . nil)
                  (paren . t)
                  (bracket . nil)
                  (bracket . t)
                  (brace . nil)
                  (brace . t)
                  (defun . nil)))

    ;; JSON - focus on structural elements.
    (js-json-mode . ((word . nil)
                     (word . t)
                     (string . nil)
                     (string . t)
                     (bracket . nil)
                     (bracket . t)
                     (brace . nil)
                     (brace . t)
                     (defun . nil)))

    ;; Org mode - text-focused with some structure.
    (org-mode . ((word . nil)
                 (word . t)
                 (string . nil)
                 (string . t)
                 (paren . nil)
                 (paren . t)
                 (bracket . nil)
                 (bracket . t)
                 (paragraph . nil)
                 (paragraph . t)
                 (defun . nil)))

    ;; Plain text - minimal structure.
    (text-mode . ((word . nil)
                  (word . t)
                  (string . nil)
                  (string . t)
                  (paren . nil)
                  (paren . t)
                  (bracket . nil)
                  (bracket . t)
                  (paragraph . nil)
                  (paragraph . t)
                  (defun . nil)))

    ;; Configuration files - key-value focused.
    (conf-mode . ((word . nil)
                  (word . t)
                  (string . nil)
                  (string . t)
                  (bracket . nil)
                  (bracket . t)
                  (paragraph . nil)
                  (paragraph . t)
                  (defun . nil)))

    ;; CMake - function-call oriented.
    (cmake-mode . ((word . nil)
                   (word . t)
                   (string . nil)
                   (string . t)
                   (paren . nil)
                   (paren . t)
                   (bracket . nil)
                   (bracket . t)
                   (brace . nil)
                   (brace . t)
                   (defun . nil)))

    ;; Windows batch files.
    (bat-mode . ((word . nil)
                 (word . t)
                 (string . nil)
                 (string . t)
                 (paren . nil)
                 (paren . t)
                 (paragraph . nil)
                 (paragraph . t)
                 (defun . nil)))

    ;; Shell scripts - rich in different delimiters.
    (sh-mode . ((word . nil)
                (word . t)
                (string . nil)
                (string . t)
                (paren . nil)
                (paren . t)
                (bracket . nil)
                (bracket . t)
                (brace . nil)
                (brace . t)
                (defun . nil))))

  "Alist of mode-specific expansion sequences.
Each sequence is a list of (OBJECT-TYPE . AROUND) pairs that define
the order in which text objects are tried when expanding the region.
AROUND is t for 'around' variants (including delimiters/whitespace),
nil for 'inner' variants (content only).

If a mode is not listed here, `davidc-region-default-expansion-sequence'
is used.")

;; Default expansion sequence used for modes without specific
;; configuration.
(defvar davidc-region-default-expansion-sequence
  '((word . nil)
    (word . t)
    (string . nil)
    (string . t)
    (paren . nil)
    (paren . t)
    (bracket . nil)
    (bracket . t)
    (brace . nil)
    (brace . t)
    (sexp . nil)
    (paragraph . nil)
    (paragraph . t)
    (defun . nil))
  "Default sequence of text objects to try when growing region.
Each element is (OBJECT-TYPE . AROUND) where AROUND is t for 'around'
variants. This is used for modes not explicitly configured in
`davidc-region-mode-expansion-sequences'.")

;;; Buffer-local variables.
(defvar-local davidc--expand-region-history nil
  "Buffer-local history of region expansions.

This is a list of cons cells (START . END) representing previous
selections, with the most recent first. Used by `davidc-region-grow'
and `davidc-region-shrink' to step through selection history.")

(defvar-local davidc--expand-region-last-point nil
  "Buffer-local record of last cursor position when expansion started.
Used to detect when the user has moved the cursor and the history
should be reset.")


;;; Mode configuration functions.
(defun davidc-region-define-text-object (mode text-object bounds-function)
  "Define a text object for a specific mode.

MODE is the major mode symbol (e.g., 'python-mode).
TEXT-OBJECT is a symbol identifying the object type (e.g., 'string).
BOUNDS-FUNCTION is a function taking optional AROUND argument and
returning (START . END) representing the bounds of the text object,
or nil if no valid object is found at point.

Example:
  (davidc-region-define-text-object 'python-mode 'string
    (lambda (around)
      ;; Custom implementation for Python strings
      ...))"
  (let ((mode-defs (alist-get mode davidc-region-mode-definitions)))
    (setf (alist-get mode davidc-region-mode-definitions)
          (cons (cons text-object bounds-function)
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

(defun davidc-region-set-mode-sequence (mode sequence)
  "Set the expansion sequence for MODE.

MODE is the major mode symbol.
SEQUENCE should be a list of (OBJECT-TYPE . AROUND) pairs
defining the order in which text objects are tried when
expanding the region.

Example:
  (davidc-region-set-mode-sequence 'latex-mode
    '((word . nil) (word . t) (math . nil) (math . t) (defun . nil)))"
  (setf (alist-get mode davidc-region-mode-expansion-sequences) sequence))


;;; Core bounds finding functions.
(defun davidc-region--word-bounds (&optional around)
  "Get bounds of word at point.

If AROUND is non-nil, include surrounding whitespace.

Returns a cons cell (START . END) or nil if no word at point.
Uses Emacs's `bounds-of-thing-at-point' for word detection."
  (when-let ((bounds (bounds-of-thing-at-point 'word)))
    (if (not around)
        bounds
      ;; Expand to include surrounding whitespace.
      (save-excursion
        (cons (progn (goto-char (car bounds))
                     (skip-chars-backward " \t")
                     (point))
              (progn (goto-char (cdr bounds))
                     (skip-chars-forward " \t")
                     (point)))))))

(defun davidc-region--symbol-bounds (&optional around)
  "Get bounds of symbol at point.

If AROUND is non-nil, include surrounding whitespace.

Returns a cons cell (START . END) or nil if no symbol at point.
Falls back to word bounds if symbol detection fails, which can
happen in modes that don't properly define symbol syntax."
  (or (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
        (if (not around)
            bounds
          ;; Expand to include surrounding whitespace.
          (save-excursion
            (cons (progn (goto-char (car bounds))
                         (skip-chars-backward " \t")
                         (point))
                  (progn (goto-char (cdr bounds))
                         (skip-chars-forward " \t")
                         (point))))))
      ;; Fallback to word bounds if symbol fails.
      (davidc-region--word-bounds around)))

(defun davidc-region--string-bounds (&optional around)
  "Get bounds of string at point.

If AROUND is non-nil, include the delimiting quotes.

Returns a cons cell (START . END) or nil if not in a string.
Uses syntax parsing to detect string boundaries accurately."
  (let ((ppss (syntax-ppss)))
    (when (nth 3 ppss)  ; Inside string.
      (let ((start (nth 8 ppss)))  ; Start of string (including quote).
        (save-excursion
          (goto-char start)
          (condition-case nil
              (progn
                (forward-sexp)  ; Jump to end of string.
                (let ((end (point)))
                  (when (> end (1+ start))  ; Valid string.
                    (if around
                        (cons start end)  ; Include quotes.
                      (cons (1+ start) (1- end))))))  ; Exclude quotes.
            (error nil)))))))

(defun davidc-region--paragraph-bounds (&optional around)
  "Get bounds of paragraph at point.

If AROUND is non-nil, include surrounding blank lines.

Returns a cons cell (START . END) or nil if no valid paragraph.
Uses Emacs's paragraph navigation commands."
  (save-excursion
    (let ((orig (point)))
      ;; Go to beginning of paragraph.
      (backward-paragraph)
      (let ((start (point)))
        ;; Go back to original position.
        (goto-char orig)
        ;; Go to end of paragraph.
        (forward-paragraph)
        (let ((end (point)))
          (when (< start end)
            (if (not around)
                (cons start end)
              ;; Include surrounding blank lines.
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
  (let ((pos (point))
        (limit (min davidc-region-delimiter-search-limit
                    (- (point-max) (point-min)))))
    (save-excursion

      ;; Strategy 1: Try fast built-in parsing first.
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

        ;; Strategy 2: Manual search when built-in parsing fails.
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
               ;; Reuse ppss when possible for performance.
               (when (< (point) (nth 0 ppss))
                 (setq ppss (syntax-ppss)))
               ;; Skip if in string or comment.
               (unless (or (nth 3 ppss) (nth 4 ppss))
                 (let ((char (char-after)))
                   (cond
                    ;; Found closing delimiter - increase nesting depth.
                    ((= char close-char) (cl-incf depth))
                    ;; Found opening delimiter.
                    ((= char open-char)
                     (if (> depth 0)
                         ;; Part of inner nesting - decrease depth.
                         (cl-decf depth)
                       ;; Found our opening delimiter.
                       (setq start (point))))))))
             ;; If we found the start, search forward for the end.
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
         ;; Get mode-specific delimiters or use defaults.
         (delimiters (or (alist-get 'delimiters
                                    (alist-get major-mode davidc-region-mode-definitions))
                         davidc-region-default-delimiters))
         (limit (min davidc-region-delimiter-search-limit
                     (- (point-max) (point-min))))
         (search-start (max (point-min) (- pos limit)))
         best-bounds best-distance)

    ;; Try each delimiter type, keeping the closest.
    (dolist (pair delimiters)
      (let ((bounds (davidc-region--find-delimiter-bounds
                     (car pair) (cdr pair) around)))
        (when bounds
          ;; Calculate distance from point to start of delimiter.
          (let ((distance (- pos (car bounds))))
            (when (and (>= distance 0)  ; Must enclose point.
                       (or (null best-distance)
                           (< distance best-distance)))
              (setq best-bounds bounds
                    best-distance distance))))))
    best-bounds))


;;; Main bounds dispatcher.
(defun davidc-region--get-bounds (text-object &optional around)
  "Get bounds for TEXT-OBJECT, dispatching to appropriate function.

TEXT-OBJECT is a symbol identifying the object type (e.g., 'word).
If AROUND is non-nil, include surrounding context.

This is the main dispatcher that:
1. Checks for mode-specific definitions
2. Falls back to built-in implementations
3. Handles Emacs thing-at-point objects

Returns a cons cell (START . END) or nil if no valid object found."
  (let ((custom-func (alist-get text-object
                                (alist-get major-mode davidc-region-mode-definitions))))
    (cond
     ;; Mode-specific custom function.
     ((functionp custom-func)
      (funcall custom-func around))
     ;; Built-in text object handlers.
     ((eq text-object 'word) (davidc-region--word-bounds around))
     ((eq text-object 'symbol) (davidc-region--symbol-bounds around))
     ((eq text-object 'string) (davidc-region--string-bounds around))
     ((eq text-object 'paragraph) (davidc-region--paragraph-bounds around))
     ((eq text-object 'paren)
      (davidc-region--find-delimiter-bounds ?\( ?\) around))
     ((eq text-object 'bracket)
      (davidc-region--find-delimiter-bounds ?\[ ?\] around))
     ((eq text-object 'brace)
      (davidc-region--find-delimiter-bounds ?\{ ?\} around))
     ((eq text-object 'delimiter) (davidc-region--any-delimiter-bounds around))
     ;; Generic Emacs thing-at-point objects.
     ((memq text-object '(sexp sentence line defun))
      (bounds-of-thing-at-point text-object))
     ;; Unknown text object.
     (t nil))))


;;; Interactive selection commands.
(defmacro davidc-region--define-selection-commands (object)
  "Define inner and around selection commands for OBJECT.

This macro generates two interactive functions:
- davidc-region-inner-OBJECT: Select the object contents
- davidc-region-around-OBJECT: Select object with delimiters/whitespace

OBJECT should be a symbol like 'word, 'string, etc."
  (let ((inner-name (intern (format "davidc-region-inner-%s" object)))
        (around-name (intern (format "davidc-region-around-%s" object)))
        (object-string (symbol-name object)))
    `(progn
       (defun ,inner-name ()
         ,(format "Select inner %s at point.
This selects the contents of the %s, excluding delimiters or quotes."
                  object-string object-string)
         (interactive)
         (if-let ((bounds (davidc-region--get-bounds ',object)))
             (progn
               (set-mark (car bounds))
               (goto-char (cdr bounds))
               (activate-mark))
           (user-error "No %s at point" ,object-string)))

       (defun ,around-name ()
         ,(format "Select around %s at point.
This selects the %s including its delimiters, quotes, or surrounding whitespace."
                  object-string object-string)
         (interactive)
         (if-let ((bounds (davidc-region--get-bounds ',object t)))
             (progn
               (set-mark (car bounds))
               (goto-char (cdr bounds))
               (activate-mark))
           (user-error "No %s at point" ,object-string))))))

;; Define all selection commands.
(davidc-region--define-selection-commands word)
(davidc-region--define-selection-commands string)
(davidc-region--define-selection-commands paren)
(davidc-region--define-selection-commands bracket)
(davidc-region--define-selection-commands brace)
(davidc-region--define-selection-commands paragraph)
(davidc-region--define-selection-commands delimiter)
(davidc-region--define-selection-commands defun)


;;; Grow/shrink implementation helpers.
(defun davidc-region--current-bounds ()
  "Get current region bounds or point.
Returns (START . END) for active region, or (POINT . POINT) if no region."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (cons (point) (point))))

(defun davidc-region--bounds-contain-p (outer inner)
  "Check if OUTER bounds fully contain INNER bounds.
Both arguments should be cons cells (START . END).
Returns t if OUTER contains INNER, nil otherwise."
  (and outer inner
       (<= (car outer) (car inner))
       (>= (cdr outer) (cdr inner))))

(defun davidc-region--bounds-equal-p (a b)
  "Check if bounds A and B are equal.
Both arguments should be cons cells (START . END).
Returns t if they represent the same region, nil otherwise."
  (and a b
       (= (car a) (car b))
       (= (cdr a) (cdr b))))

(defun davidc-region--get-expansion-sequence ()
  "Get the expansion sequence for the current mode.
Returns the mode-specific sequence if defined, otherwise returns
the default expansion sequence."
  (or (alist-get major-mode davidc-region-mode-expansion-sequences)
      davidc-region-default-expansion-sequence))

(defun davidc-region--find-next-expansion (current-bounds)
  "Find the next valid expansion from CURRENT-BOUNDS.

Tries each text object in the mode's expansion sequence to find
the next larger selection that contains the current selection.

Returns (SPEC . BOUNDS) where SPEC is (OBJECT-TYPE . AROUND)
and BOUNDS is (START . END), or nil if no expansion found."
  (let* ((pos (if (= (car current-bounds) (cdr current-bounds))
                  (point)  ; No selection, use cursor position
                (car current-bounds)))  ; Use start of selection
         (sequence (davidc-region--get-expansion-sequence))
         result)

    ;; Try each object type in sequence.
    (catch 'found
      (dolist (spec sequence)
        (let* ((object-type (car spec))
               (around (cdr spec))
               bounds)
          ;; Get bounds for this text object.
          (condition-case nil
              (save-excursion
                (goto-char pos)
                (setq bounds (davidc-region--get-bounds object-type around)))
            (error nil))

          ;; Check if this is a valid expansion.
          (when (and bounds
                     ;; Must contain current selection.
                     (davidc-region--bounds-contain-p bounds current-bounds)
                     ;; Must be larger than current selection.
                     (not (davidc-region--bounds-equal-p bounds current-bounds)))
            (setq result (cons spec bounds))
            (throw 'found t)))))

    result))

;;; Main grow/shrink functions.
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
  (let* ((current-bounds (davidc-region--current-bounds))
         (current-point (point))
         (starting-fresh (= (car current-bounds) (cdr current-bounds))))

    ;; Reset history if starting fresh or cursor moved.
    (when (or starting-fresh
              (not (equal davidc--expand-region-last-point current-point)))
      (setq davidc--expand-region-history nil
            davidc--expand-region-last-point current-point))

    ;; Find next expansion.
    (let ((expansion (davidc-region--find-next-expansion current-bounds)))
      (cond
       (expansion
        ;; Save current bounds to history (what we're expanding FROM).
        (push current-bounds davidc--expand-region-history)

        ;; Apply the expansion.
        (let ((new-bounds (cdr expansion)))
          (set-mark (car new-bounds))
          (goto-char (cdr new-bounds))
          (activate-mark)

          ;; Report what we selected.
          (let ((type-info (car expansion)))
            (message "Selected %s %s"
                     (if (cdr type-info) "around" "inner")
                     (symbol-name (car type-info))))))

       (t
        (message "No expansion found"))))))

(defun davidc-region-shrink ()
  "Shrink the selected region to the previous smaller text object.

This reverses the effect of `davidc-region-grow' by stepping back
through the expansion history. If there's no history, deactivates
the current selection.

Can be called repeatedly to step back through multiple expansions."
  (interactive)
  (cond
   ((not (region-active-p))
    (message "No active region to shrink"))

   ;; We have history - restore previous selection.
   (davidc--expand-region-history
    (let ((previous-bounds (pop davidc--expand-region-history)))
      ;; Check if previous bounds was just a point (no selection).
      (if (= (car previous-bounds) (cdr previous-bounds))
          ;; It was just a cursor position - deactivate selection.
          (progn
            (deactivate-mark)
            (goto-char (car previous-bounds))
            (message "Back to cursor"))
        ;; It was a selection - restore it.
        (progn
          (set-mark (car previous-bounds))
          (goto-char (cdr previous-bounds))
          (activate-mark)
          (message "Restored previous selection")))))

   ;; No history - just deactivate.
   (t
    (deactivate-mark)
    (message "No shrink history"))))


;;; Mode-specific customizations.
(defun davidc-region--python-string-bounds (&optional around)
  "Python-specific string bounds including triple quotes.

Handles both regular strings (using default syntax parsing) and
Python's triple-quoted strings (''' or \"\"\").

If AROUND is non-nil, includes the quote characters."
  (let* ((ppss (syntax-ppss))
         (string-start-after-quotes (+ (nth 8 ppss) 1)))
    ;; Only proceed if we're actually inside a string.
    (when (and (nth 3 ppss) string-start-after-quotes)
      (save-excursion
        ;; The syntax parser gives us the position after the opening quotes.
        ;;
        ;; We need to look backward to find the actual quotes.
        (goto-char string-start-after-quotes)

        ;; Look backward for the opening quotes.
        (let ((outside-start nil)
              (inside-start nil)
              (quote-length 1)) ; default to single quote.

          ;; Check for triple quotes before the reported string start.
          (when (>= (point) 3)
            (backward-char 3)
            (when (looking-at "\\('''\\|\"\"\"\\)")
              (setq inside-start string-start-after-quotes)
              (setq outside-start (- string-start-after-quotes 4))
              (setq quote-length 3)))

          ;; If not triple quotes, check for single quotes.
          (unless outside-start
            (goto-char string-start-after-quotes)
            (backward-char 1)
            (message "test: %s" (looking-at "\\(['\"]\\)"))
            (when (looking-at "\\(['\"]\\)")
              (setq inside-start string-start-after-quotes)
              (setq outside-start (- string-start-after-quotes 2))
              (setq quote-length 1)))

          ;; Now find the end of the string.
          (when outside-start
            (goto-char (- inside-start 1))
            (condition-case nil
                (progn
                  (forward-sexp)
                  (let ((string-end (point)))
                    (if around
                        ;; Include the quotes.
                        (cons (+ outside-start 1) (+ string-end (- quote-length 1)))
                      ;; Exclude the quotes.
                      (cons inside-start
                            (- string-end 1)))))
              (error nil))))))))


;; Set up Python customization when python-mode is loaded.
(with-eval-after-load 'python
  (davidc-region-define-text-object 'python-mode 'string
                                    #'davidc-region--python-string-bounds))

(provide 'davidc-region)
