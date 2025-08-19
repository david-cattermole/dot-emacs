;;; -*- lexical-binding: t -*-
;;
;; Markdown major mode with syntax highlighting.
;;
;; This mode provides Markdown editing with support for:
;; * Theme-compatible syntax highlighting using standard font-lock faces.
;; * Basic indentation and text formatting.
;; * Syntax highlighting for headers, emphasis, code, links, lists, etc.
;; * Auto-mode integration for .md and .markdown files.
;;

(require 'font-lock)

(defgroup davidc-markdown nil
  "Markdown editing mode with comprehensive syntax highlighting."
  :group 'languages
  :prefix "davidc-markdown-")

(defcustom davidc-markdown-indent-offset 4
  "Default indentation offset for Markdown.
This controls how much to indent nested structures."
  :type 'integer
  :group 'davidc-markdown)

(defcustom davidc-markdown-enable-math nil
  "When non-nil, enable LaTeX math highlighting in Markdown."
  :type 'boolean
  :group 'davidc-markdown)

;; Font lock keywords for Markdown syntax highlighting.
;; Using standard font-lock faces for theme compatibility.
;; Order is important - more specific patterns should come first.
(defun davidc-markdown--get-font-lock-keywords ()
  "Generate font-lock keywords dynamically based on configuration."
  `(
    ;; Fenced code blocks - opening/closing delimiters with language spec.
    ("^\\(```\\|~~~\\)\\(.*\\)$"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))

    ;; Code blocks (indented by 4+ spaces or 1+ tabs).
    ;;
    ;; Only match if line doesn't start with list markers or blockquote markers.
    ("^\\(    \\|\t\\)\\([^ \t>*+0-9-].*\\)$" 2 font-lock-string-face)

    ;; Headers - ATX style (# ## ### etc.) with optional trailing #.
    ("^\\(#+\\)\\s-+\\(.*?\\)\\s-*\\(#+\\)?\\s-*$"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     (3 font-lock-keyword-face nil t))

    ;; Setext-style header underlines (= and - lines).
    ("^\\s-*\\(=+\\)\\s-*$" 1 font-lock-keyword-face)
    ("^\\s-*\\(-+\\)\\s-*$" 1 font-lock-keyword-face)

    ;; Horizontal rules.
    ("^\\s-*\\(\\*\\s-*\\*\\s-*\\*\\|\\*\\s-*\\*\\s-*\\*.*\\)\\s-*$" 1 font-lock-keyword-face)
    ("^\\s-*\\(-\\s-*-\\s-*-\\|-\\s-*-\\s-*-.*\\)\\s-*$" 1 font-lock-keyword-face)
    ("^\\s-*\\(_\\s-*_\\s-*_\\|_\\s-*_\\s-*_.*\\)\\s-*$" 1 font-lock-keyword-face)

    ;; Bold+Italic text - ***text*** and ___text___ (must come first).
    ;;
    ;; Use word boundaries to prevent conflicts with other patterns.
    ("\\(?:^\\|[^*]\\)\\(\\*\\*\\*\\)\\([^*\n]+?\\)\\(\\*\\*\\*\\)\\(?:[^*]\\|$\\)"
     (1 font-lock-warning-face)
     (2 font-lock-warning-face)
     (3 font-lock-warning-face))
    ("\\(?:^\\|\\W\\)\\(___\\)\\([^_\n]+?\\)\\(___\\)\\(?:\\W\\|$\\)"
     (1 font-lock-warning-face)
     (2 font-lock-warning-face)
     (3 font-lock-warning-face))

    ;; Bold text - **text** and __text__
    ;;
    ;; Use word boundaries to prevent conflicts.
    ("\\(?:^\\|[^*]\\)\\(\\*\\*\\)\\([^*\n]+?\\)\\(\\*\\*\\)\\(?:[^*]\\|$\\)"
     (1 font-lock-builtin-face)
     (2 font-lock-builtin-face)
     (3 font-lock-builtin-face))
    ("\\(?:^\\|\\W\\)\\(__\\)\\([^_\n]+?\\)\\(__\\)\\(?:\\W\\|$\\)"
     (1 font-lock-builtin-face)
     (2 font-lock-builtin-face)
     (3 font-lock-builtin-face))

    ;; Italic text - *text* and _text_ with proper word boundary handling.
    ;;
    ;; Asterisk version: avoid matching within words like file*name*here.
    ("\\(?:^\\|[^_*]\\)\\(\\*\\)\\([^*\n]+?\\)\\(\\*\\)\\(?:[^_*]\\|$\\)"
     (1 font-lock-variable-name-face)
     (2 font-lock-variable-name-face)
     (3 font-lock-variable-name-face))
    ;; Underscore version: use word boundaries to avoid snake_case
    ;; conflicts.
    ("\\(?:^\\|\\W\\)\\(_\\)\\([^_\n]+?\\)\\(_\\)\\(?:\\W\\|$\\)"
     (1 font-lock-variable-name-face)
     (2 font-lock-variable-name-face)
     (3 font-lock-variable-name-face))

    ;; Inline code - `code`
    ("\\(`\\)\\([^`\n]+\\)\\(`\\)"
     (1 font-lock-string-face)
     (2 font-lock-string-face)
     (3 font-lock-string-face))

    ;; Links - [text](url) and [text][ref]
    ("\\(\\[\\)\\([^]]+\\)\\(\\]\\)\\(\\(\\[\\)\\([^]]*\\)\\(\\]\\)\\)"
     (1 font-lock-type-face)
     (2 font-lock-type-face)
     (3 font-lock-type-face)
     (5 font-lock-type-face)
     (6 font-lock-type-face)
     (7 font-lock-type-face))
    ("\\(\\[\\)\\([^]]+\\)\\(\\]\\)\\((\\)\\([^)]+\\)\\()\\)"
     (1 font-lock-type-face)
     (2 font-lock-type-face)
     (3 font-lock-type-face)
     (4 font-lock-type-face)
     (5 font-lock-type-face)
     (6 font-lock-type-face))

    ;; Reference-style link definitions.
    ("^\\s-*\\(\\[\\)\\([^]]+\\)\\(\\]\\)\\s-*:\\s-*\\(.*\\)$"
     (1 font-lock-type-face)
     (2 font-lock-type-face)
     (3 font-lock-type-face)
     (4 font-lock-type-face))

    ;; Auto-links - <url>
    ("\\(<\\)\\([^<>\n]+\\)\\(>\\)"
     (1 font-lock-type-face)
     (2 font-lock-type-face)
     (3 font-lock-type-face))

    ;; Lists - unordered (-, *, +)
    ("^\\s-*\\([-*+]\\)\\s-+" 1 font-lock-builtin-face)

    ;; Lists - ordered (1. 2. etc.)
    ("^\\s-*\\([0-9]+\\.\\)\\s-+" 1 font-lock-builtin-face)

    ;; Blockquotes
    ("^\\s-*\\(>\\)\\(.*\\)$"
     (1 font-lock-comment-face)
     (2 font-lock-comment-face))

    ;; HTML comments (simplified for reliability)
    ("\\(<!--\\)\\(.*?\\)\\(-->\\)"
     (1 font-lock-comment-delimiter-face)
     (2 font-lock-comment-face)
     (3 font-lock-comment-delimiter-face))

    ;; HTML tags (basic support)
    ("\\(</?\\)\\([a-zA-Z0-9]+\\)\\([^>]*\\)\\(>\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-keyword-face)
     (3 font-lock-keyword-face)
     (4 font-lock-keyword-face))
    ;; Math highlighting (if enabled)
    ,@(when davidc-markdown-enable-math
        '(("\\(\\$\\$\\)\\([^$]+\\)\\(\\$\\$\\)"
           (1 font-lock-constant-face)
           (2 font-lock-constant-face)
           (3 font-lock-constant-face))
          ("\\(\\$\\)\\([^$\n]+\\)\\(\\$\\)"
           (1 font-lock-constant-face)
           (2 font-lock-constant-face)
           (3 font-lock-constant-face))))
    ))

;; Syntax table for Markdown
(defvar davidc-markdown-mode-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    ;; Underscores are word constituents in Markdown
    (modify-syntax-entry ?_ "w" table)

    ;; Asterisks and other punctuation
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?# "." table)
    (modify-syntax-entry ?` "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?\[ "." table)
    (modify-syntax-entry ?\] "." table)
    (modify-syntax-entry ?\( "." table)
    (modify-syntax-entry ?\) "." table)

    table)
  "Syntax table for Markdown mode.")

;; Helper functions for Markdown editing
(defun davidc-markdown-insert-header (level)
  "Insert a Markdown header at the specified LEVEL (1-6)."
  (interactive "nHeader level (1-6): ")
  (when (and (>= level 1) (<= level 6))
    (beginning-of-line)
    (insert (make-string level ?#) " ")
    (when (eolp)
      (save-excursion (newline)))))

(defun davidc-markdown-insert-bold ()
  "Insert bold markup (**) around region or at point."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert "**")
        (goto-char start)
        (insert "**"))
    (insert "****")
    (backward-char 2)))

(defun davidc-markdown-insert-italic ()
  "Insert italic markup (*) around region or at point."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert "*")
        (goto-char start)
        (insert "*"))
    (insert "**")
    (backward-char 1)))

(defun davidc-markdown-insert-code ()
  "Insert inline code markup (`) around region or at point."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert "`")
        (goto-char start)
        (insert "`"))
    (insert "``")
    (backward-char 1)))

(defun davidc-markdown-insert-link ()
  "Insert a Markdown link."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring (region-beginning) (region-end))))
        (delete-region (region-beginning) (region-end))
        (insert "[" text "]()"))
    (insert "[]()")
    (backward-char 3)))

;; Keymap for Markdown mode
(defvar davidc-markdown-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-h") 'davidc-markdown-insert-header)
    (define-key map (kbd "C-c C-b") 'davidc-markdown-insert-bold)
    (define-key map (kbd "C-c C-i") 'davidc-markdown-insert-italic)
    (define-key map (kbd "C-c C-c") 'davidc-markdown-insert-code)
    (define-key map (kbd "C-c C-l") 'davidc-markdown-insert-link)
    (define-key map (kbd "RET") 'newline-and-indent)
    map)
  "Keymap for Markdown mode.")

;; Define the major mode
;;;###autoload
(define-derived-mode davidc-markdown-mode text-mode "Markdown"
  "Major mode for editing Markdown files with syntax highlighting.

This mode provides Markdown editing support including:
- Theme-compatible syntax highlighting using standard font-lock faces
- Support for headers, emphasis, code, links, lists, blockquotes
- Basic HTML tag and comment highlighting
- Optional LaTeX math highlighting
- Utility functions for common Markdown editing tasks

Key bindings:
\\{davidc-markdown-mode-map}

Customization:
- `davidc-markdown-indent-offset': Controls indentation amount (default: 4)
- `davidc-markdown-enable-math': Enable LaTeX math highlighting (default: nil)"

  ;; Set up syntax highlighting
  (setq font-lock-defaults
        `((davidc-markdown--get-font-lock-keywords)
          nil                           ; KEYWORDS-ONLY
          nil                           ; CASE-FOLD
          nil                           ; SYNTAX-ALIST
          nil))                         ; SYNTAX-BEGIN

  ;; Set up other mode configurations
  (setq-local tab-width davidc-markdown-indent-offset)
  (setq-local indent-tabs-mode nil)
  (setq-local comment-start "<!-- ")
  (setq-local comment-end " -->")
  (setq-local comment-start-skip "<!--[ \t]*")
  (setq-local comment-end-skip "[ \t]*-->")

  ;; Enable automatic paragraph filling
  (setq-local fill-column 80)
  (setq-local paragraph-start "\\s-*$\\|\\s-*[#>*+-]\\|\\s-*[0-9]+\\.")
  (setq-local paragraph-separate "\\s-*$")

  ;; Force immediate fontification
  (font-lock-mode 1)

  ;; Ensure fontification is applied
  (when (fboundp 'font-lock-flush)
    (font-lock-flush))
  (if (fboundp 'font-lock-ensure)
      (font-lock-ensure)
    (with-no-warnings (font-lock-fontify-buffer))))

;; Auto-mode setup
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.md\\'" . davidc-markdown-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . davidc-markdown-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . davidc-markdown-mode))

(provide 'davidc-markdown-mode)
