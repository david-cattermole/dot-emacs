;;; -*- lexical-binding: t -*-
;;
;; Code folding with outline-minor-mode
;;
;; Inspired by: https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
;;
;; This implementation provides:
;; - Smart cycling of fold visibility (local and global)
;; - Language-specific outline patterns (Python, C/C++, Rust, MEL)
;; - Optional auto-folding on file open

;;; Toggle Functions.

(defun davidc-outline-toggle-global ()
  "Toggle visibility for entire buffer between folded and unfolded.
- If any content is hidden: show all
- If all content is visible: hide all bodies"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((has-hidden nil))
      ;; Check if anything is hidden.
      (while (and (not has-hidden) (not (eobp)))
        (when (outline-invisible-p)
          (setq has-hidden t))
        (forward-line 1))
      ;; Toggle based on what we found.
      (if has-hidden
          (outline-show-all)
        (outline-hide-body)))))

(defun davidc-outline-toggle ()
  "Toggle fold state at point.
- If on a heading and content is hidden: show full subtree.
- If on a heading and content is visible: hide body but keep child headings visible.
- If not on a heading: do nothing."
  (interactive)
  (when (and (bound-and-true-p outline-minor-mode)
             (outline-on-heading-p))
    (if (save-excursion
          (outline-end-of-heading)
          (outline-invisible-p))
        ;; Currently folded - show everything under this heading.
        (outline-show-subtree)
      ;; Currently visible - hide body but keep children visible.
      (let ((parent-level (funcall outline-level)))
        (save-excursion
          ;; First show the entire subtree to reset state.
          (outline-show-subtree)
          ;; Hide this entry's body.
          (outline-hide-entry)
          ;; Move to first child (if any).
          (outline-next-heading)
          ;; Hide bodies of all descendants.
          (while (and (not (eobp))
                      (> (funcall outline-level) parent-level))
            (outline-hide-entry)
            (outline-next-heading)))))))

;;; Keybinding Setup.

(defun davidc-outline-setup-keybindings ()
  "Set up outline-minor-mode keybindings for prog-mode.
Only applies in prog-mode derived buffers to avoid conflicts."
  (when (derived-mode-p 'prog-mode)
    (local-set-key (kbd "<tab>") 'davidc-outline-toggle)
    (local-set-key (kbd "<backtab>") 'davidc-outline-toggle-global)))

;;; Auto-Fold Setup.

(defun davidc-outline-auto-fold-setup ()
  "Automatically fold all blocks when outline-minor-mode is enabled.
This runs after the buffer is loaded and outline patterns are configured.
Only folds if davidc-config-use-auto-fold is enabled."
  (when (and (bound-and-true-p outline-minor-mode)
             davidc-config-use-auto-fold)
    ;; Capture the current buffer for the timer
    (let ((buf (current-buffer)))
      ;; Use run-with-idle-timer to ensure outline-regexp is set by language hooks
      (run-with-idle-timer 0.1 nil
                           (lambda ()
                             (when (buffer-live-p buf)
                               (with-current-buffer buf
                                 (outline-hide-body))))))))

;;; Language-Specific Setup Functions.

(defun davidc-outline-python-setup ()
  "Configure outline-minor-mode for Python.
Folds:
- Class definitions (class ...)
- Function definitions (def ..., async def ...)
- Decorators (@...)

Does NOT fold:
- try/except blocks
- loops (for, while)
- if statements
- other control structures

Outline level is determined by indentation."
  (setq-local outline-regexp
              (rx (or
                   ;; Class and function definitions
                   (seq line-start (* space)
                        bow (or "class" "def" "async") eow)
                   ;; Decorators
                   (seq line-start (* space) "@"))))

  ;; Use indentation to determine outline level
  (setq-local outline-level
              (lambda ()
                (1+ (/ (current-indentation) python-indent-offset))))

  ;; Auto-fold if enabled
  (davidc-outline-auto-fold-setup))

(defun davidc-outline-c-setup ()
  "Configure outline-minor-mode for C/C++.
Folds brace-delimited blocks including:
- Functions
- Classes and structs
- Namespaces
- Conditional blocks

Uses simple brace matching to identify foldable regions."
  ;; Match lines ending with opening brace
  (setq-local outline-regexp "[^\n]*{[ \t]*$")

  ;; Level based on indentation
  (setq-local outline-level
              (lambda ()
                (1+ (/ (current-indentation) c-basic-offset))))

  ;; Auto-fold if enabled
  (davidc-outline-auto-fold-setup))

(defun davidc-outline-rust-setup ()
  "Configure outline-minor-mode for Rust.
Folds brace-delimited blocks including:
- Functions (fn)
- Implementations (impl)
- Traits
- Modules (mod)
- Structs and enums

Uses keyword detection for Rust constructs."
  (setq-local outline-regexp
              (rx line-start (* space)
                  ;; Optional visibility and qualifiers
                  (? (or "pub" "pub(crate)" "pub(super)") (+ space))
                  (? (or "async" "const" "unsafe" "extern") (+ space))
                  ;; Main keywords
                  (or "fn" "impl" "trait" "mod" "struct" "enum" "type" "const" "static")
                  (or space "(")
                  ))

  ;; Level based on indentation
  (setq-local outline-level
              (lambda ()
                (1+ (/ (current-indentation) rust-indent-offset))))

  ;; Auto-fold if enabled
  (davidc-outline-auto-fold-setup))

(defun davidc-outline-mel-setup ()
  "Configure outline-minor-mode for MEL (Maya Embedded Language).
MEL uses C-like syntax, so we use brace-based folding similar to C mode."
  ;; MEL is C-like, use same approach as C
  (setq-local outline-regexp "[^\n]*{[ \t]*$")
  (setq-local outline-level
              (lambda ()
                (1+ (/ (current-indentation) c-basic-offset))))

  ;; Auto-fold if enabled
  (davidc-outline-auto-fold-setup))

(provide 'davidc-outline)
