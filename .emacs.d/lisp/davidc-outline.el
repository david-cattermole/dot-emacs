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
- If all content is visible: hide to show only level 1 headings"
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
        (hide-sublevels 1)))))

(defun davidc-outline-toggle ()
  "Cycle through fold states: folded → children → subtree → folded.
- Folded: Only the heading is visible, all content and children hidden.
- Children: Heading and immediate child headings visible (child bodies hidden).
- Subtree: Everything visible (all descendants and their content).

Press TAB repeatedly to cycle through these states."
  (interactive)
  (when (and (bound-and-true-p outline-minor-mode)
             (outline-on-heading-p))
    (let ((parent-level (funcall outline-level)))
      (cond
       ;; State 1: Check if completely folded (content after heading is invisible)
       ((save-excursion
          (outline-end-of-heading)
          (outline-invisible-p))
        ;; Folded → Show children
        (outline-show-entry)
        (outline-show-children))

       ;; State 2/3: Content is visible, determine which state by checking first child
       ((save-excursion
          (outline-next-heading)
          (and (not (eobp))
               (> (funcall outline-level) parent-level)
               ;; First child exists, check if its body is hidden
               (progn
                 (outline-end-of-heading)
                 (outline-invisible-p))))
        ;; State 2: Children visible but folded → Show subtree
        (outline-show-subtree))

       ;; State 3: Subtree fully visible → Hide all
       (t
        (outline-hide-subtree))))))

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
  "Configure outline-minor-mode for Python with keyword-based hierarchical folding.
Folds all definitions and control structures:
- Functions (def, async def), classes, decorators (@)
- Control structures (if/elif/else, for, while, try/except/finally, with, match/case)

Hierarchical structure:
- Functions/classes/decorators: always level 1 (regardless of indentation)
- Control structures: level 2+ (based on indentation: 2 + indentation/indent-offset)

This ensures the top outline level shows all functions, methods, and classes (but no conditionals).
Conditionals become visible when you expand their containing functions."
  (setq-local outline-regexp
              (rx line-start (* space)
                  (or
                   ;; Primary headings: class, def, async def, decorators
                   (seq bow (or "class" "def" "async") eow)
                   (seq "@")
                   ;; Control structures
                   (seq bow (or "if" "elif" "else"
                                "for" "while"
                                "try" "except" "finally"
                                "with" "match" "case") eow))))

  ;; Use keyword-based levels: def/class/decorators at level 1, conditionals at 2+
  (setq-local outline-level
              (lambda ()
                (let ((is-def-class-or-decorator
                       (save-excursion
                         (beginning-of-line)
                         (looking-at (rx (* space)
                                         (or
                                          (seq bow (or "class" "def" "async") eow)
                                          (seq "@")))))))
                  (if is-def-class-or-decorator
                      1
                    (+ 2 (/ (current-indentation) python-indent-offset))))))

  ;; Auto-fold if enabled
  (davidc-outline-auto-fold-setup))

(defun davidc-outline-c-setup ()
  "Configure outline-minor-mode for C/C++ with keyword-based hierarchical folding.
Folds all brace-delimited blocks including:
- Functions, classes, structs, namespaces
- Control structures (if/else/for/while/do/switch)

Hierarchical structure:
- Functions/classes/structs/namespaces: always level 1 (regardless of indentation)
- Control structures: level 2+ (based on indentation: 2 + indentation/c-basic-offset)

This ensures the top outline level shows all functions and classes (but no control structures).
Control structures become visible when you expand their containing functions."
  ;; Match lines ending with opening brace
  (setq-local outline-regexp "[^\n]*{[ \t]*$")

  ;; Use keyword-based levels: functions/classes at level 1, control structures at 2+
  (setq-local outline-level
              (lambda ()
                (let ((is-control-structure
                       (save-excursion
                         (beginning-of-line)
                         (looking-at (rx (* space)
                                         (or
                                          ;; Control keywords
                                          (seq bow (or "if" "else" "for" "while"
                                                       "do" "switch") eow)
                                          ;; Control with parens
                                          (seq bow (or "if" "for" "while") eow
                                               (* space) "(")))))))
                  (if is-control-structure
                      (+ 2 (/ (current-indentation) c-basic-offset))
                    1))))

  ;; Auto-fold if enabled
  (davidc-outline-auto-fold-setup))

(defun davidc-outline-rust-setup ()
  "Configure outline-minor-mode for Rust with keyword-based hierarchical folding.
Folds all blocks including:
- Functions (fn), implementations (impl), traits, modules (mod), structs, enums
- Control structures (if/else/for/while/loop/match)

Hierarchical structure:
- Functions/impl/traits/structs/enums/mods: always level 1 (regardless of indentation)
- Control structures: level 2+ (based on indentation: 2 + indentation/rust-indent-offset)

This ensures the top outline level shows all functions, impls, traits, and structs (but no control structures).
Control structures become visible when you expand their containing functions."
  (setq-local outline-regexp
              (rx line-start (* space)
                  (or
                   ;; Function-level constructs
                   (seq (? (or "pub" "pub(crate)" "pub(super)") (+ space))
                        (? (or "async" "const" "unsafe" "extern") (+ space))
                        (or "fn" "impl" "trait" "mod" "struct" "enum"
                            "type" "const" "static")
                        (or space "("))
                   ;; Control structures
                   (seq bow (or "if" "else" "for" "while" "loop"
                                "match") eow))))

  ;; Use keyword-based levels: fn/impl/trait/struct at level 1, control structures at 2+
  (setq-local outline-level
              (lambda ()
                (let ((is-control-structure
                       (save-excursion
                         (beginning-of-line)
                         (looking-at (rx (* space)
                                         bow (or "if" "else" "for" "while"
                                                 "loop" "match") eow)))))
                  (if is-control-structure
                      (+ 2 (/ (current-indentation) rust-indent-offset))
                    1))))

  ;; Auto-fold if enabled
  (davidc-outline-auto-fold-setup))

(defun davidc-outline-mel-setup ()
  "Configure outline-minor-mode for MEL (Maya Embedded Language).
MEL uses C-like syntax with brace-based folding.

Hierarchical structure:
- All brace-delimited blocks: level based on indentation (1 + indentation/c-basic-offset)
- Simple indentation-based hierarchy for all block types"
  ;; MEL is C-like, use same approach as C
  (setq-local outline-regexp "[^\n]*{[ \t]*$")
  (setq-local outline-level
              (lambda ()
                (1+ (/ (current-indentation) c-basic-offset))))

  ;; Auto-fold if enabled
  (davidc-outline-auto-fold-setup))

(provide 'davidc-outline)
