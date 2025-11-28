;;; -*- lexical-binding: t -*-
;;
;; YAML major mode with syntax highlighting.
;;
;; This mode provides YAML editing with support for:
;; * Theme-compatible syntax highlighting using standard font-lock faces.
;; * Intelligent indentation for all YAML constructs.
;; * Smart electric backspace and formatting.
;; * Comments, keys, values, strings, numbers, booleans, null values.
;; * Document separators, list indicators, block scalars.
;; * Anchors, aliases, tags, flow sequences/mappings.
;; * Timestamps and other YAML data types.
;; * imenu integration and utility functions.
;;

(require 'font-lock)

(defgroup davidc-yaml nil
  "YAML editing mode with comprehensive syntax highlighting."
  :group 'languages
  :prefix "davidc-yaml-")

(defcustom davidc-yaml-indent-offset 2
  "Default indentation offset for YAML.
This controls how much to indent nested structures."
  :type 'integer
  :group 'davidc-yaml)

(defcustom davidc-yaml-backspace-function 'backward-delete-char-untabify
  "Function called by `davidc-yaml-electric-backspace' when deleting backwards.
Can be customized to use different deletion behaviors."
  :type 'function
  :group 'davidc-yaml)

(defcustom davidc-yaml-auto-insert-document-separator t
  "When non-nil, automatically insert document separators in empty buffers."
  :type 'boolean
  :group 'davidc-yaml)

;; Font lock keywords for YAML syntax highlighting.
;; Using standard font-lock faces for theme compatibility.
;; Order is important - more specific patterns should come first.
(defvar davidc-yaml-font-lock-keywords
  `(
    ;; Comments - highest priority to override other patterns.
    ("\\(#.*\\)$" 1 font-lock-comment-face t)

    ;; Document separators - must be at start of line.
    ("^\\(---\\)\\(\\s-.*\\)?$" 1 font-lock-keyword-face)
    ("^\\(\\.\\.\\.\\)\\(\\s-.*\\)?$" 1 font-lock-keyword-face)

    ;; Block scalar indicators with optional parameters.
    ("^\\(\\s-*\\)\\([|>]\\)\\([-+]?\\)\\([0-9]*\\)\\(\\s-*\\)$"
     (2 font-lock-keyword-face) (3 font-lock-keyword-face) (4 font-lock-keyword-face))

    ;; Anchors and aliases - must come before other patterns to avoid
    ;; conflicts.
    ("\\(&[a-zA-Z0-9_][a-zA-Z0-9_-]*\\)" 1 font-lock-function-name-face)
    ("\\(\\*[a-zA-Z0-9_][a-zA-Z0-9_-]*\\)" 1 font-lock-function-name-face)

    ;; YAML tags - comprehensive pattern for all tag types.
    ("\\(!\\(?:!\\|[a-zA-Z0-9_-]+!\\)?[a-zA-Z0-9_/-]*\\)" 1 font-lock-type-face)

    ;; Quoted strings with proper escape sequence handling.
    ("\\(\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"\\)" 1 font-lock-string-face)
    ("\\('\\(?:[^'\\\\]\\|\\\\.\\)*'\\)" 1 font-lock-string-face)

    ;; Flow sequences and mappings.
    ("\\([\\[\\]{}]\\)" 1 font-lock-builtin-face)

    ;; List indicators - improved pattern that doesn't require space
    ;; after dash.
    ("^\\(\\s-*\\)\\(-\\)\\(\\s-\\|$\\)" 2 font-lock-builtin-face)

    ;; Keys - improved pattern with better context awareness.
    ;;
    ;; Matches key: value patterns more accurately.
    ("^\\(\\s-*\\)\\([^][\n#:{}\"'|>]+?\\)\\(\\s-*\\):\\(\\s-\\|$\\)"
     2 font-lock-variable-name-face)

    ;; Numbers - comprehensive pattern for all number formats.
    ;;
    ;; Includes integers, floats, scientific notation, hex, octal, binary.
    ("\\<\\([-+]?\\(?:0x[0-9a-fA-F]+\\|0o[0-7]+\\|0b[01]+\\|[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][-+]?[0-9]+\\)?\\)\\)\\>"
     1 font-lock-constant-face)

    ;; Boolean values - all YAML boolean variants.
    ("\\<\\(true\\|false\\|yes\\|no\\|on\\|off\\|True\\|False\\|TRUE\\|FALSE\\|YES\\|NO\\|On\\|Off\\|ON\\|OFF\\)\\>"
     1 font-lock-constant-face)

    ;; Null values - all YAML null variants.
    ("\\<\\(null\\|Null\\|NULL\\|~\\)\\>" 1 font-lock-constant-face)

    ;; ISO 8601 timestamp formats.
    ("\\<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?:[T ][0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\(?:\\.[0-9]+\\)?\\(?:Z\\|[+-][0-9]\\{2\\}:[0-9]\\{2\\}\\)?\\)?\\)\\>"
     1 font-lock-constant-face)
    )
  "Font lock keywords for YAML mode using standard font-lock faces for theme compatibility.")


;; Syntax table for YAML.
(defvar davidc-yaml-mode-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    ;; Comments start with # and go to end of line
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)

    ;; Word constituents for YAML identifiers
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?- "w" table)

    ;; Punctuation characters
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?> "." table)

    ;; Balanced expressions.
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)

    table)
  "Syntax table for YAML mode.")

;; Helper functions for indentation analysis.
(defun davidc-yaml--current-line-empty-p ()
  "Return t if current line is empty or contains only whitespace."
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\s-*$")))

(defun davidc-yaml--current-line-comment-p ()
  "Return t if current line contains only a comment (possibly with leading whitespace)."
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\s-*#")))

(defun davidc-yaml--get-previous-meaningful-line ()
  "Move to the previous non-empty, non-comment line and return its indentation.
Returns nil if no such line is found (e.g., at beginning of buffer)."
  (let ((found nil)
        (indent 0))
    (save-excursion
      (while (and (not found) (not (bobp)))
        (forward-line -1)
        (unless (or (davidc-yaml--current-line-empty-p)
                    (davidc-yaml--current-line-comment-p))
          (setq found t
                indent (current-indentation)))))
    (when found indent)))

(defun davidc-yaml--line-has-key-p ()
  "Return t if current line appears to contain a YAML key."
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\s-*[^][\n#:{}\"'|>]+?\\s-*:")))

(defun davidc-yaml--line-has-list-item-p ()
  "Return t if current line is a YAML list item (starts with dash)."
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\s-*-\\(\\s-\\|$\\)")))

(defun davidc-yaml--line-has-block-scalar-p ()
  "Return t if current line has a block scalar indicator (| or >)."
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\s-*.*[|>]\\s-*$")))

(defun davidc-yaml--in-block-scalar-p ()
  "Return t if point is currently inside a block scalar.
This function looks backwards to find a block scalar indicator at 
an appropriate indentation level."
  (save-excursion
    (let ((current-indent (current-indentation))
          (found-scalar nil))
      ;; Look backwards for a block scalar indicator.
      (while (and (not found-scalar) (not (bobp)))
        (forward-line -1)
        (when (and (not (davidc-yaml--current-line-empty-p))
                   (not (davidc-yaml--current-line-comment-p)))
          (let ((line-indent (current-indentation)))
            (cond
             ;; Found a block scalar at lesser or equal indentation.
             ((and (<= line-indent current-indent)
                   (davidc-yaml--line-has-block-scalar-p))
              (setq found-scalar t))
             ;; Found content at lesser indentation - we're not in a block scalar.
             ((< line-indent current-indent)
              (setq found-scalar nil)
              (goto-char (point-min)))))))
      found-scalar)))

(defun davidc-yaml--line-is-document-separator-p ()
  "Return t if current line is a document separator (--- or ...)."
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\s-*\\(---\\|\\.\\.\\.\\)")))

;; Main indentation function.
(defun davidc-yaml-indent-line ()
  "Indent current line as YAML.
This function provides intelligent indentation based on YAML structure,
including support for keys, list items, block scalars, and document separators."
  (interactive)
  (let* ((cur-indent (current-indentation))
         (point-pos (point))
         (line-start (line-beginning-position))
         (point-offset (- point-pos line-start))
         new-indent)

    (save-excursion
      (beginning-of-line)
      (cond
       ;; Document separators always at column 0.
       ((davidc-yaml--line-is-document-separator-p)
        (setq new-indent 0))

       ;; If we're in a block scalar, maintain current indentation.
       ((davidc-yaml--in-block-scalar-p)
        (setq new-indent cur-indent))

       ;; Empty line - use previous meaningful line's indentation.
       ((davidc-yaml--current-line-empty-p)
        (setq new-indent (or (davidc-yaml--get-previous-meaningful-line) 0)))

       ;; Default case - calculate based on previous line structure.
       (t
        (let ((prev-indent (davidc-yaml--get-previous-meaningful-line)))
          (if (null prev-indent)
              (setq new-indent 0)
            (save-excursion
              (forward-line -1)
              (cond
               ;; Previous line is a key - increase indent for value.
               ((davidc-yaml--line-has-key-p)
                (setq new-indent (+ prev-indent davidc-yaml-indent-offset)))

               ;; Previous line is a list item.
               ((davidc-yaml--line-has-list-item-p)
                (if (davidc-yaml--line-has-list-item-p)
                    ;; Current line is also a list item - same level.
                    (setq new-indent prev-indent)
                  ;; Current line is not a list item - indent under it.
                  (setq new-indent (+ prev-indent 2))))

               ;; Previous line has block scalar - increase indent.
               ((davidc-yaml--line-has-block-scalar-p)
                (setq new-indent (+ prev-indent davidc-yaml-indent-offset)))

               ;; Default - same as previous line.
               (t
                (setq new-indent prev-indent)))))

          ;; Adjust for current line being a list item.
          (when (and (davidc-yaml--line-has-list-item-p)
                     (> new-indent davidc-yaml-indent-offset))
            (save-excursion
              (forward-line -1)
              (unless (davidc-yaml--line-has-list-item-p)
                (setq new-indent (- new-indent davidc-yaml-indent-offset)))))))))

    ;; Apply the indentation.
    (when new-indent
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to new-indent)

      ;; Restore point position relative to text.
      (when (> point-offset cur-indent)
        (goto-char (+ (line-beginning-position)
                     (max new-indent (- point-offset cur-indent new-indent))))))))

;; Electric backspace function for smart dedentation.
(defun davidc-yaml-electric-backspace (arg)
  "Delete backwards with smart indentation.
With ARG, delete that many characters backwards.
If at beginning of line with only whitespace, performs smart dedentation."
  (interactive "*p")
  (if (and (= arg 1)
           (looking-back "^\\s-+" (line-beginning-position))
           (> (current-indentation) 0))
      ;; Smart dedent by one indent level.
      (let* ((current-indent (current-indentation))
             (new-indent (max 0 (- current-indent davidc-yaml-indent-offset))))
        (beginning-of-line)
        (delete-horizontal-space)
        (when (> new-indent 0)
          (indent-to new-indent)))
    ;; Normal backspace.
    (funcall davidc-yaml-backspace-function arg)))

;; Keymap for YAML mode.
(defvar davidc-yaml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'newline-and-indent)
    (define-key map (kbd "TAB") 'davidc-yaml-indent-line)
    (define-key map (kbd "DEL") 'davidc-yaml-electric-backspace)
    (define-key map (kbd "C-c C-d") 'davidc-yaml-insert-document-separator)
    (define-key map (kbd "C-c C-l") 'davidc-yaml-insert-list-item)
    map)
  "Keymap for YAML mode.")

;; Utility functions.
(defun davidc-yaml-insert-document-separator ()
  "Insert a YAML document separator (---) with proper spacing."
  (interactive)
  (beginning-of-line)
  (unless (looking-at "^\\s-*$")
    (newline))
  (insert "---")
  (newline-and-indent))

(defun davidc-yaml-insert-list-item ()
  "Insert a YAML list item (-) with proper indentation."
  (interactive)
  (beginning-of-line)
  (indent-according-to-mode)
  (insert "- ")
  (when (eolp)
    (save-excursion (newline-and-indent))))

(defun davidc-yaml-fold-all ()
  "Fold all YAML blocks (requires outline-minor-mode or similar)."
  (interactive)
  (when (bound-and-true-p outline-minor-mode)
    (outline-hide-body)))

(defun davidc-yaml-unfold-all ()
  "Unfold all YAML blocks (requires outline-minor-mode or similar)."
  (interactive)
  (when (bound-and-true-p outline-minor-mode)
    (outline-show-all)))

;; Function to handle paragraph filling in YAML context.
(defun davidc-yaml-fill-paragraph (&optional justify)
  "Fill paragraph in YAML context, respecting YAML structure.
Optional argument JUSTIFY specifies justification."
  (save-excursion
    (let ((paragraph-start "\\s-*$\\|\\s-*#")
          (paragraph-separate "\\s-*$"))
      (fill-paragraph justify))))

;; Define the major mode.
;;;###autoload
(define-derived-mode davidc-yaml-mode text-mode "YAML"
  "Major mode for editing YAML files with syntax highlighting.

This mode provides YAML editing support including:
- Theme-compatible syntax highlighting using standard font-lock faces.
- Intelligent indentation for all YAML constructs  .
- Smart electric backspace for easy dedentation.
- Support for comments, keys, values, strings, numbers, booleans.
- Document separators, list indicators, block scalars.
- Anchors, aliases, tags, flow sequences/mappings.
- ISO 8601 timestamps and other YAML data types.
- imenu integration for navigation.
- Utility functions for common YAML editing tasks.

Key bindings:
\\{davidc-yaml-mode-map}

The mode provides context-aware indentation that understands YAML
structure including nested mappings, sequences, block scalars, and
document boundaries.

Customization:
- `davidc-yaml-indent-offset': Controls indentation amount (default: 2)
- `davidc-yaml-backspace-function': Function for electric backspace
- `davidc-yaml-auto-insert-document-separator': Auto-insert --- in empty files"
  
  ;; Set up syntax highlighting with corrected font-lock-defaults.
  (setq font-lock-defaults
        '(davidc-yaml-font-lock-keywords
          nil                           ; KEYWORDS-ONLY
          nil                           ; CASE-FOLD
          nil                           ; SYNTAX-ALIST
          nil))                         ; SYNTAX-BEGIN - remove the problematic function setting

  ;; Set up other mode configurations.
  (setq-local indent-line-function 'davidc-yaml-indent-line)
  (setq-local tab-width davidc-yaml-indent-offset)
  (setq-local indent-tabs-mode nil)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-end "")
  
  ;; imenu setup.
  (setq-local imenu-generic-expression
              '(("Keys" "^\\s-*\\([^][\n#:{}\"'|>]+?\\)\\s-*:" 1)
                ("Anchors" "^\\s-*\\(&[a-zA-Z0-9_-]+\\)" 1)))

  ;; Auto-insert document separator.
  (when (and davidc-yaml-auto-insert-document-separator
             (= (point-min) (point-max)))
    (insert "---\n"))

  ;; Force immediate fontification.
  (font-lock-mode 1)
  
  ;; Flush any existing fontification and ensure new fontification.
  (when (fboundp 'font-lock-flush)
    (font-lock-flush))
    
  (if (fboundp 'font-lock-ensure)
      (font-lock-ensure)
    (with-no-warnings (font-lock-fontify-buffer))))

;; Auto-mode setup.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . davidc-yaml-mode))

;; Optional: Add to magic-mode-alist for files starting with "---"
;;;###autoload
(add-to-list 'magic-mode-alist '("\\`---" . davidc-yaml-mode))

(provide 'davidc-yaml-mode)
