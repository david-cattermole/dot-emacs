;;; davidc-outline.el --- Code folding with outline-minor-mode  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Code folding configuration using outline-minor-mode.
;;
;; Inspired by: https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
;;
;; This implementation provides hierarchical code folding with smart visibility
;; cycling and language-specific patterns. The key insight is using a two-level
;; hierarchy: top-level constructs (functions, classes) at level 1, and nested
;; elements (control structures, comments) at level 2+. This ensures that the
;; overview mode shows only major structural elements.
;;
;; Features:
;; - Smart three-state cycling (Folded -> Children -> Subtree -> repeat)
;; - Language-specific folding patterns (Python, C/C++, Rust, MEL)
;; - Generic comment block folding (only folds the first line of multi-line blocks)
;; - Auto-folding on file open (optional via `davidc-config-use-auto-fold`)
;; - TAB to cycle local fold state, Shift-TAB to toggle global visibility

;;; Code:

(require 'outline)
(require 'rx)

;;; Variables

(defvar-local davidc-outline-comment-starter nil
  "Regex matching the start of a line comment for multi-line block detection.
Should match the comment prefix pattern (e.g., \"#\" for Python, \"//\" for C/Rust).

When non-nil, this enables generic comment continuation filtering: consecutive
lines matching this pattern with the same indentation are treated as a single
foldable block, with only the first line marked as a heading. This prevents
each comment line from appearing as a separate foldable item.

If nil, all comment lines matching `outline-regexp` are treated as separate headings.

Example values:
  Python:  (rx (* space) \"#\")
  C/Rust:  (rx (* space) \"//\")
  MEL:     nil (no comment continuation filtering needed)")

;;; Toggle Functions

(defun davidc-outline-toggle-global ()
  "Toggle visibility for entire buffer between overview and expanded states.

Detection logic:
- Scans buffer to check if any outline heading has hidden content
- Uses `outline-on-heading-p` to find headings (respects comment continuation filtering)

Toggle behavior:
- If any content is hidden: show all (expand everything)
- If all content is visible: hide to overview mode (show only level 1 headings)

In overview mode, only top-level constructs (functions, classes, etc.) are visible,
with all nested content (control structures, comments, function bodies) hidden."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((has-hidden nil))
      ;; Check if any outline heading has hidden content
      (while (and (not has-hidden) (not (eobp)))
        (when (and (outline-on-heading-p)
                   (save-excursion
                     (outline-end-of-heading)
                     (outline-invisible-p)))
          (setq has-hidden t))
        (outline-next-heading))
      ;; Toggle based on current state
      (if has-hidden
          (outline-show-all)
        (davidc-outline--hide-sublevels-custom 1)))))

(defun davidc-outline--hide-sublevels-custom (levels)
  "Hide everything except headings at or above LEVELS.

Algorithm (two-step approach):
1. Call `outline-hide-body` to hide all body text, keeping all headings visible
2. Iterate through headings and hide those deeper than LEVELS

This differs from built-in `hide-sublevels` by using our advised
`outline-on-heading-p`, which filters out comment continuation lines.
This ensures multi-line comment blocks are treated as single foldable units.

Example with LEVELS=1:
- Level 1 headings (functions, classes): visible
- Level 2+ headings (control structures, comments): hidden
- All body text: hidden"
  (save-excursion
    ;; First hide all body text, keeping headings visible
    (outline-hide-body)
    ;; Then hide headings that are above the requested level
    (goto-char (point-min))
    (while (not (eobp))
      (when (outline-on-heading-p)
        (let ((heading-level (funcall outline-level)))
          (if (> heading-level levels)
              ;; Hide headings deeper than requested level
              (outline-hide-subtree)
            ;; Keep headings at or above requested level visible
            (outline-show-heading))))
      (outline-next-heading))))

(defun davidc-outline-toggle ()
  "Cycle through fold states: Folded -> Children -> Subtree -> Folded.

Three-state cycling behavior (press TAB repeatedly):

1. Folded (initial state):
   - Only the heading line is visible
   - All content and child headings are hidden
   -> Press TAB: Show Children

2. Children visible:
   - Heading and immediate child headings are visible
   - Child heading bodies remain hidden
   - Own body content is visible
   -> Press TAB: Show Subtree

3. Subtree visible:
   - Everything visible: heading, body, all descendants and their content
   -> Press TAB: Hide all (return to Folded state)

State detection:
- Checks visibility at end of heading to determine current state
- For Children vs Subtree: checks if first child's body is hidden"
  (interactive)
  (when (and (bound-and-true-p outline-minor-mode)
             (outline-on-heading-p))
    (let ((parent-level (funcall outline-level)))
      (cond
       ;; Folded -> Show Children
       ((save-excursion
          (outline-end-of-heading)
          (outline-invisible-p))
        (outline-show-entry)
        (outline-show-children))
       ;; Children visible -> Show Subtree
       ((save-excursion
          (outline-next-heading)
          (and (not (eobp))
               (> (funcall outline-level) parent-level)
               (progn
                 (outline-end-of-heading)
                 (outline-invisible-p))))
        (outline-show-subtree))
       ;; Subtree visible -> Hide all
       (t
        (outline-hide-subtree))))))

;;; Comment Folding Support
;;
;; Generic approach to comment block folding (replaces 3 language-specific functions):
;; - Language setup sets `davidc-outline-comment-starter` to match comment prefix.
;; - `davidc-outline--comment-continuation-p` checks if a line continues a comment block.
;; - Advice on `outline-on-heading-p` filters out continuation lines.
;; - Result: Only the first line of multi-line comment blocks is treated as a heading.
;;
;; This prevents each comment line from appearing as a separate foldable item,
;; while still allowing single-line comments and comment block starts to be folded.

(defun davidc-outline--comment-continuation-p ()
  "Return t if current line is a continuation of a comment block.

A line is a continuation if it and the previous line both match
`davidc-outline-comment-starter` with the same indentation.

Why this matters:
Without continuation filtering, each comment line would be a separate foldable
heading. With filtering, only the first line of a multi-line comment block is
treated as a heading, making the outline structure cleaner and more useful.

Returns nil if `davidc-outline-comment-starter` is nil (filtering disabled)."
  (when davidc-outline-comment-starter
    (save-excursion
      (beginning-of-line)
      (when (looking-at davidc-outline-comment-starter)
        (let ((current-indent (current-indentation)))
          (and (zerop (forward-line -1))
               (looking-at davidc-outline-comment-starter)
               (= (current-indentation) current-indent)))))))

(defun davidc-outline--on-heading-p-advice (orig-fun &rest args)
  "Advice to filter out comment continuation lines from being treated as headings.

Strategy:
1. Call the original `outline-on-heading-p` function
2. If it returns t, check if current line is a comment continuation
3. If it is a continuation, return nil (not a heading)
4. Otherwise, return the original result

This advice integrates seamlessly with all outline-mode functions, ensuring
multi-line comment blocks are treated as single foldable units throughout."
  (let ((result (apply orig-fun args)))
    (if (and result (davidc-outline--comment-continuation-p))
        nil
      result)))

(advice-add 'outline-on-heading-p :around #'davidc-outline--on-heading-p-advice)

;;; Auto-Fold

(defun davidc-outline-auto-fold-setup ()
  "Automatically fold buffer if `davidc-config-use-auto-fold` is non-nil.

This function is called by language setup functions after configuring
`outline-regexp` and `outline-level`. It uses an idle timer to ensure
the outline patterns are fully configured before folding.

Timing strategy:
- Uses `run-with-idle-timer` with 0.1 second delay
- Ensures language hooks have finished setting up outline-regexp
- Only folds if buffer is still alive (handles rapid buffer switching)

Folding behavior:
- Calls `outline-hide-body` to hide all body text
- Keeps all headings visible (no level filtering)
- User can then use Shift-TAB to toggle to overview mode (level 1 only)"
  (when (and (bound-and-true-p outline-minor-mode)
             (bound-and-true-p davidc-config-use-auto-fold))
    (let ((buf (current-buffer)))
      (run-with-idle-timer 0.1 nil
                           (lambda ()
                             (when (buffer-live-p buf)
                               (with-current-buffer buf
                                 (outline-hide-body))))))))

;;; Language Setup

(defun davidc-outline-python-setup ()
  "Configure outline-minor-mode for Python with hierarchical folding.

Foldable elements:
- Functions (def, async def), classes, decorators (@) - Level 1
- Control structures (if/elif/else, for, while, try/except/finally, with, match/case) - Level 2+
- Triple-quoted docstrings (\"\"\" or ''') - Level 2+
- Multi-line # comment blocks - Level 2+ (only first line is a heading)

Hierarchical structure:
- Level 1: Functions, classes, decorators (regardless of indentation)
- Level 2+: Control structures, docstrings, comments (based on indentation)

This ensures the top outline level (overview) shows only functions, methods, and
classes, with all control flow and comments nested inside. Press TAB on a function
to see its internal structure (conditionals, loops, comments)."
  (setq-local outline-regexp
              (rx line-start (* space)
                  (or (seq bow (or "class" "def" "async") eow)
                      (seq "@")
                      (seq bow (or "if" "elif" "else" "for" "while"
                                   "try" "except" "finally" "with" "match" "case") eow)
                      (or "\"\"\"" "'''")
                      (seq "#"))))
  (setq-local outline-level
              (lambda ()
                (save-excursion
                  (beginning-of-line)
                  ;; Level 1 for definitions, Level 2+ for everything else
                  (if (looking-at (rx (* space) (or (seq bow (or "class" "def" "async") eow) (seq "@"))))
                      1
                    (+ 2 (/ (current-indentation) python-indent-offset))))))
  (setq-local davidc-outline-comment-starter (rx (* space) "#"))
  (davidc-outline-auto-fold-setup))

(defun davidc-outline-c-setup ()
  "Configure outline-minor-mode for C/C++ with hierarchical folding.

Foldable elements:
- Functions, classes, structs, namespaces (lines ending with {) - Level 1
- Control structures (if/else/for/while/do/switch with {) - Level 2+
- Block comments (/* */ and /** */) - Level 2+
- Multi-line // comment blocks - Level 2+ (only first line is a heading)

Hierarchical structure:
- Level 1: Functions, classes, structs, namespaces (regardless of indentation)
- Level 2+: Control structures and comments (based on indentation)

This ensures the top outline level (overview) shows only functions and classes,
with all control flow and comments nested inside. Press TAB on a function to see
its internal structure (if statements, loops, comments)."
  (setq-local outline-regexp
              (rx (or (seq (*? nonl) "{" (* space) eol)
                      (seq line-start (* space) (or "//" "/*")))))
  (setq-local outline-level
              (lambda ()
                (save-excursion
                  (beginning-of-line)
                  (if (looking-at (rx (* space) (or (seq bow (or "if" "else" "for" "while" "do" "switch") eow)
                                                    "//" "/*")))
                      (+ 2 (/ (current-indentation) c-basic-offset))
                    1))))
  (setq-local davidc-outline-comment-starter (rx (* space) "//"))
  (davidc-outline-auto-fold-setup))

(defun davidc-outline-rust-setup ()
  "Configure outline-minor-mode for Rust with hierarchical folding.

Foldable elements:
- Functions (fn), implementations (impl), traits, modules (mod), structs, enums - Level 1
- Control structures (if/else/for/while/loop/match) - Level 2+
- Block comments (/* */) - Level 2+
- Doc comments (/// and //!) - Level 2+
- Multi-line // comment blocks - Level 2+ (only first line is a heading)

Hierarchical structure:
- Level 1: Functions, impl blocks, traits, structs, enums, mods (regardless of indentation)
- Level 2+: Control structures and comments (based on indentation)

This ensures the top outline level (overview) shows only functions, impl blocks,
traits, and data structures, with all control flow and comments nested inside.
Press TAB on a function to see its internal structure (match statements, loops, comments)."
  (setq-local outline-regexp
              (rx line-start (* space)
                  (or (seq (? (or "pub" "pub(crate)" "pub(super)") (+ space))
                           (? (or "async" "const" "unsafe" "extern") (+ space))
                           (or "fn" "impl" "trait" "mod" "struct" "enum" "type" "const" "static")
                           (or space "("))
                      (seq bow (or "if" "else" "for" "while" "loop" "match") eow)
                      (seq (or "/*" "///" "//!" ))
                      (seq "//"))))
  (setq-local outline-level
              (lambda ()
                (save-excursion
                  (beginning-of-line)
                  (if (looking-at (rx (* space) (or (seq bow (or "if" "else" "for" "while" "loop" "match") eow)
                                                    "//" "/*" "///" "//!" )))
                      (+ 2 (/ (current-indentation) rust-indent-offset))
                    1))))
  (setq-local davidc-outline-comment-starter (rx (* space) "//"))
  (davidc-outline-auto-fold-setup))

(defun davidc-outline-mel-setup ()
  "Configure outline-minor-mode for MEL (Maya Embedded Language).

MEL uses C-like syntax with brace-based folding. Unlike Python/C/Rust,
MEL uses simple indentation-based hierarchy for all block types.

Foldable elements:
- All brace-delimited blocks (functions, control structures, etc.)

Hierarchical structure:
- Level = 1 + (indentation / c-basic-offset)
- Simple indentation-based hierarchy (no special treatment of functions vs conditionals)

This simpler approach works well for MEL's scripting nature where the distinction
between top-level and nested constructs is less important."
  (setq-local outline-regexp "[^\n]*{[ \t]*$")
  (setq-local outline-level
              (lambda () (1+ (/ (current-indentation) c-basic-offset))))
  (davidc-outline-auto-fold-setup))

;;; Keybindings

(defun davidc-outline-setup-keybindings ()
  "Set up outline keybindings for prog-mode buffers.

Keybindings:
- CTRL-TAB (C-<tab>): Cycle through fold states for current heading
  (Folded -> Children -> Subtree -> Folded)
- Shift-TAB (<backtab>): Toggle global visibility for entire buffer
  (Overview mode <-> Fully expanded)

Scope limitation:
Only applies in prog-mode derived buffers to avoid conflicts with other
modes that use TAB for different purposes (e.g., org-mode, text-mode)."
  (when (derived-mode-p 'prog-mode)
    (local-set-key (kbd "C-<tab>") 'davidc-outline-toggle)
    (local-set-key (kbd "<backtab>") 'davidc-outline-toggle-global)))

(provide 'davidc-outline)
;;; davidc-outline.el ends here
