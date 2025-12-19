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
Uses custom logic to skip continuation comment lines.
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
        ;; Use custom logic instead of hide-sublevels
        (davidc-outline--hide-sublevels-custom 1)))))

(defun davidc-outline--hide-sublevels-custom (levels)
  "Hide everything except headings at or above LEVELS.
Custom implementation that respects outline-on-heading-p (includes our advice).
Unlike built-in hide-sublevels, this explicitly checks outline-on-heading-p
for each potential heading to skip continuation comment lines."
  (save-excursion
    ;; Step 1: Show everything (clear overlays)
    (outline-show-all)

    ;; Step 2: Iterate and selectively hide
    (goto-char (point-min))
    (let ((regexp (concat "^\\(?:" outline-regexp "\\)")))
      (while (re-search-forward regexp nil t)
        (beginning-of-line)
        ;; Step 3: Only process real headings
        (when (outline-on-heading-p)
          (let ((level (funcall outline-level)))
            ;; Step 4: Hide entire subtree, then show heading if at/below threshold
            ;; This ensures child headings are also hidden for level 1 headings
            (outline-hide-subtree)
            (when (<= level levels)
              ;; Show just the heading line (body and children stay hidden)
              (outline-show-heading))))
        ;; Move past this line to continue searching
        (end-of-line)))))

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

;;; Helper Functions for Comment Detection.

(defvar-local davidc-outline--comment-folding-enabled nil
  "Buffer-local flag indicating davidc-outline comment folding is active.
This is used to ensure advice only runs in buffers where we've configured
custom comment folding patterns.")

(defun davidc-outline--python-multiline-comment-p ()
  "Check if current line is the FIRST line of a multi-line # comment block.
Returns t if this is the opening line of a multi-line comment block, nil otherwise.
A line is considered the opening line if:
- Current line is a # comment
- Next line is also a # comment with same indentation
- Previous line is NOT a # comment (or has different indentation)"
  (save-excursion
    (beginning-of-line)
    (when (looking-at (rx (* space) "#"))
      (let ((current-indent (current-indentation)))
        ;; Check if previous line is NOT part of same comment block
        (let ((prev-is-comment
               (save-excursion
                 (and (= (forward-line -1) 0)  ; Successfully moved to previous line
                      (looking-at (rx (* space) "#"))
                      (= (current-indentation) current-indent)))))
          ;; Only proceed if previous line is not a matching comment
          (and (not prev-is-comment)
               ;; Check if next line IS part of same comment block
               (save-excursion
                 (forward-line 1)
                 (and (not (eobp))
                      (looking-at (rx (* space) "#"))
                      (= (current-indentation) current-indent)))))))))

(defun davidc-outline--c-multiline-comment-p ()
  "Check if current line is the FIRST line of a multi-line // comment block.
Returns t if this is the opening line of a multi-line comment block, nil otherwise.
A line is considered the opening line if:
- Current line is a // comment
- Next line is also a // comment with same indentation
- Previous line is NOT a // comment (or has different indentation)"
  (save-excursion
    (beginning-of-line)
    (when (looking-at (rx (* space) "//"))
      (let ((current-indent (current-indentation)))
        ;; Check if previous line is NOT part of same comment block
        (let ((prev-is-comment
               (save-excursion
                 (and (= (forward-line -1) 0)  ; Successfully moved to previous line
                      (looking-at (rx (* space) "//"))
                      (= (current-indentation) current-indent)))))
          ;; Only proceed if previous line is not a matching comment
          (and (not prev-is-comment)
               ;; Check if next line IS part of same comment block
               (save-excursion
                 (forward-line 1)
                 (and (not (eobp))
                      (looking-at (rx (* space) "//"))
                      (= (current-indentation) current-indent)))))))))

(defun davidc-outline--rust-multiline-comment-p ()
  "Check if current line is the FIRST line of a multi-line // comment block.
Excludes doc comments (/// and //!) which are always foldable.
Returns t if this is the opening line of a multi-line comment block, nil otherwise.
A line is considered the opening line if:
- Current line is a regular // comment (not /// or //!)
- Next line is also a // comment with same indentation
- Previous line is NOT a // comment (or has different indentation)"
  (save-excursion
    (beginning-of-line)
    (when (looking-at (rx (* space) "//" (not (any "/" "!"))))
      (let ((current-indent (current-indentation)))
        ;; Check if previous line is NOT part of same comment block
        (let ((prev-is-comment
               (save-excursion
                 (and (= (forward-line -1) 0)  ; Successfully moved to previous line
                      (looking-at (rx (* space) "//"))
                      (= (current-indentation) current-indent)))))
          ;; Only proceed if previous line is not a matching comment
          (and (not prev-is-comment)
               ;; Check if next line IS part of same comment block
               (save-excursion
                 (forward-line 1)
                 (and (not (eobp))
                      (looking-at (rx (* space) "//"))
                      (= (current-indentation) current-indent)))))))))

(defun davidc-outline--comment-continuation-p ()
  "Check if current line is a continuation (non-first) line of a comment block.
This checks for # or // comments that are part of a multi-line block but not the first line."
  (save-excursion
    (beginning-of-line)
    (let ((is-python-comment (looking-at (rx (* space) "#")))
          (is-c-comment (looking-at (rx (* space) "//" (not (any "/" "!")))))
          (is-rust-doc-comment (looking-at (rx (* space) (or "///" "//!" "/*")))))
      (when (or is-python-comment is-c-comment (and is-rust-doc-comment (not (looking-at (rx (* space) "/*")))))
        (let ((current-indent (current-indentation)))
          ;; Check if previous line IS part of same comment block
          ;; If yes, then this is a continuation line
          (save-excursion
            (and (= (forward-line -1) 0)
                 (cond
                  (is-python-comment
                   (and (looking-at (rx (* space) "#"))
                        (= (current-indentation) current-indent)))
                  ((or is-c-comment is-rust-doc-comment)
                   (and (looking-at (rx (* space) "//"))
                        (= (current-indentation) current-indent)))
                  (t nil)))))))))

(defun davidc-outline--on-heading-p-advice (orig-fun &rest args)
  "Advice to make outline-on-heading-p return nil for continuation comment lines."
  (let ((result (apply orig-fun args)))
    (if (and result
             (bound-and-true-p davidc-outline--comment-folding-enabled)
             (davidc-outline--comment-continuation-p))
        nil  ; Not a heading - it's a continuation line
      result)))  ; Is a heading


;;; Install advice globally (runs once when package loads).

(advice-add 'outline-on-heading-p :around #'davidc-outline--on-heading-p-advice)

;;; Language-Specific Setup Functions.

(defun davidc-outline-python-setup ()
  "Configure outline-minor-mode for Python with keyword-based hierarchical folding.
Folds all definitions, control structures, and comments/docstrings:
- Functions (def, async def), classes, decorators (@)
- Control structures (if/elif/else, for, while, try/except/finally, with, match/case)
- Triple-quoted docstrings (\"\"\" or ''')
- Multi-line # comment blocks (2+ consecutive lines)

Hierarchical structure:
- Functions/classes/decorators: always level 1 (regardless of indentation)
- Control structures and comments: level 2+ (based on indentation: 2 + indentation/indent-offset)

This ensures the top outline level shows all functions, methods, and classes (but no conditionals or comments).
Conditionals and comments become visible when you expand their containing functions."
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
                                "with" "match" "case") eow)
                   ;; Triple-quoted docstrings
                   (seq (or "\"\"\"" "'''"))
                   ;; # comments (will filter single-line in outline-level)
                   (seq "#"))))

  ;; Use keyword-based levels: def/class/decorators at level 1, conditionals/comments at 2+
  (setq-local outline-level
              (lambda ()
                (save-excursion
                  (beginning-of-line)
                  (cond
                   ;; Level 1: def/class/decorators
                   ((looking-at (rx (* space)
                                    (or (seq bow (or "class" "def" "async") eow)
                                        (seq "@"))))
                    1)
                   ;; Docstrings: level 2+ based on indentation
                   ((and (looking-at (rx (* space) (or "\"\"\"" "'''")))
                         (not (nth 3 (syntax-ppss)))) ; not already in string
                    (+ 2 (/ (current-indentation) python-indent-offset)))
                   ;; # comments: level 2+ (advice filters continuation lines)
                   ((looking-at (rx (* space) "#"))
                    (+ 2 (/ (current-indentation) python-indent-offset)))
                   ;; Control structures: level 2+
                   (t (+ 2 (/ (current-indentation) python-indent-offset)))))))

  ;; Enable comment folding advice for this buffer
  (setq-local davidc-outline--comment-folding-enabled t)

  ;; Auto-fold if enabled
  (davidc-outline-auto-fold-setup))

(defun davidc-outline-c-setup ()
  "Configure outline-minor-mode for C/C++ with keyword-based hierarchical folding.
Folds all brace-delimited blocks and comments including:
- Functions, classes, structs, namespaces
- Control structures (if/else/for/while/do/switch)
- Block comments (/* */ and /** */)
- Multi-line // comment blocks (2+ consecutive lines)

Hierarchical structure:
- Functions/classes/structs/namespaces: always level 1 (regardless of indentation)
- Control structures and comments: level 2+ (based on indentation: 2 + indentation/c-basic-offset)

This ensures the top outline level shows all functions and classes (but no control structures or comments).
Control structures and comments become visible when you expand their containing functions."
  ;; Match lines ending with opening brace OR comment lines
  (setq-local outline-regexp
              (rx (or
                   ;; Brace-delimited blocks
                   (seq (*? nonl) "{" (* space) eol)
                   ;; Block comments /* and doc comments /**
                   (seq line-start (* space) "/*")
                   ;; Line comments // (will filter single-line in outline-level)
                   (seq line-start (* space) "//"))))

  ;; Use keyword-based levels: functions/classes at level 1, control structures/comments at 2+
  (setq-local outline-level
              (lambda ()
                (save-excursion
                  (beginning-of-line)
                  (cond
                   ;; Block comments /* or doc comments /**
                   ((looking-at (rx (* space) "/*"))
                    (+ 2 (/ (current-indentation) c-basic-offset)))
                   ;; Line comments // (advice filters continuation lines)
                   ((looking-at (rx (* space) "//"))
                    (+ 2 (/ (current-indentation) c-basic-offset)))
                   ;; Control structures
                   ((looking-at (rx (* space)
                                    (or
                                     ;; Control keywords
                                     (seq bow (or "if" "else" "for" "while"
                                                  "do" "switch") eow)
                                     ;; Control with parens
                                     (seq bow (or "if" "for" "while") eow
                                          (* space) "("))))
                    (+ 2 (/ (current-indentation) c-basic-offset)))
                   ;; Functions/classes: level 1
                   (t 1)))))

  ;; Enable comment folding advice for this buffer
  (setq-local davidc-outline--comment-folding-enabled t)

  ;; Auto-fold if enabled
  (davidc-outline-auto-fold-setup))

(defun davidc-outline-rust-setup ()
  "Configure outline-minor-mode for Rust with keyword-based hierarchical folding.
Folds all blocks and comments including:
- Functions (fn), implementations (impl), traits, modules (mod), structs, enums
- Control structures (if/else/for/while/loop/match)
- Block comments (/* */)
- Doc comments (/// and //!)
- Multi-line // comment blocks (2+ consecutive lines)

Hierarchical structure:
- Functions/impl/traits/structs/enums/mods: always level 1 (regardless of indentation)
- Control structures and comments: level 2+ (based on indentation: 2 + indentation/rust-indent-offset)

This ensures the top outline level shows all functions, impls, traits, and structs (but no control structures or comments).
Control structures and comments become visible when you expand their containing functions."
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
                                "match") eow)
                   ;; Block comments and doc comments
                   (seq (or "/*" "///" "//!"))
                   ;; Line comments // (will filter single-line in outline-level)
                   (seq "//"))))

  ;; Use keyword-based levels: fn/impl/trait/struct at level 1, control structures/comments at 2+
  (setq-local outline-level
              (lambda ()
                (save-excursion
                  (beginning-of-line)
                  (cond
                   ;; Block comments /* or doc comments /// //!
                   ((looking-at (rx (* space) (or "/*" "///" "//!")))
                    (+ 2 (/ (current-indentation) rust-indent-offset)))
                   ;; Regular line comments // (advice filters continuation lines)
                   ((looking-at (rx (* space) "//" (not (any "/" "!"))))
                    (+ 2 (/ (current-indentation) rust-indent-offset)))
                   ;; Control structures
                   ((looking-at (rx (* space)
                                    bow (or "if" "else" "for" "while"
                                            "loop" "match") eow))
                    (+ 2 (/ (current-indentation) rust-indent-offset)))
                   ;; Functions/impl/traits/structs: level 1
                   (t 1)))))

  ;; Enable comment folding advice for this buffer
  (setq-local davidc-outline--comment-folding-enabled t)

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
