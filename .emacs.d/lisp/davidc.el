;;; -*- lexical-binding: t -*-
;; A simple package for personal usage and learning.
;;
;;
;; http://xahlee.info/emacs/emacs/elisp_editing_basics.html
;; http://xahlee.info/emacs/emacs/elisp_examples.html


(defun davidc-open-init-file ()
  "Open init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))


(defun davidc-open-package-file ()
  "Open davidc.el file."
  (interactive)
  (find-file "~/.emacs.d/lisp/davidc.el"))


(defun davidc-open-custom-vars ()
  "Open custom-vars.el file."
  (interactive)
  ;; *custom-vars-file* is a global variable.
  (find-file *custom-vars-file*))


(defun davidc--read-string (prompt initial-input)
  "Read a string from the minibuffer, with delete-selection-mode enabled."
  (minibuffer-with-setup-hook
      (lambda ()
        ;; If you enable Delete Selection (minor) mode, then inserting
        ;; text while the mark is active causes the selected text to
        ;; be deleted first. This also deactivates the mark. Many
        ;; graphical applications follow this convention, but Emacs
        ;; does not.
        (delete-selection-mode 1))

    (read-string prompt initial-input)))

(defun davidc--rename-symbol-core (text-to-replace start end &optional no-whole-symbol-only)
  "Core function for renaming symbols with shared logic.
TEXT-TO-REPLACE is the text to find and replace.
START and END define the region to operate in (nil means whole buffer).
NO-WHOLE-SYMBOL-ONLY, if non-nil, matches fragments inside larger symbols.

This function handles the common logic for all symbol renaming operations:
1. Constructs the appropriate search pattern based on whether to match whole symbols.
2. Prompts for a replacement string with the original text pre-selected.
3. Performs interactive query-replace within the specified region.
4. Returns to the original cursor position after completion."
  (let* ((original-point (point))
         ;; By default, only match whole symbols. With prefix arg, match fragments too.
         (search-pattern (if no-whole-symbol-only
                             (regexp-quote text-to-replace)
                           (concat "\\_<" (regexp-quote text-to-replace) "\\_>")))
         ;; Pre-populate the prompt with the current text for editing.
         (replacement (davidc--read-string
                      (format "Replace '%s' with: " text-to-replace)
                      text-to-replace))
         ;; If region boundaries are nil, use buffer boundaries.
         (region-start (or start (point-min)))
         (region-end (or end (point-max))))

    ;; Only proceed if the replacement is different from the original text.
    (if (string-equal text-to-replace replacement)
        (message "No changes made to '%s'" text-to-replace)

      ;; Deactivate the mark to avoid confusion during replacement.
      (when (use-region-p)
        (deactivate-mark))

      ;; Go to the beginning of the region to search from.
      (goto-char region-start)

      ;; Perform the query-replace operation.
      (unwind-protect
          (query-replace-regexp search-pattern replacement nil region-start region-end)
        ;; This will be executed when query-replace-regexp exits (even if by error).
        (goto-char original-point)

        (message "Rename completed and returned to original position")))))

(defun davidc-rename-symbol-in-buffer (&optional no-whole-symbol-only)
  "Rename occurrences of the selected text or symbol at point, with confirmation.
By default, only matches whole symbols. With a prefix argument (C-u),
matches fragments inside larger symbols as well.

If a region is selected, replaces all occurrences of the selected text.
If no region is selected, identifies the symbol under the cursor.

Then performs an interactive query-replace, allowing you to confirm or
reject each replacement. The prompt is pre-filled with the current text
for easy editing. After the operation completes, it returns to the
original cursor position.

When you invoke this function:
1. It identifies the text to replace (from selection or symbol at point).
2. Prompts you for a replacement string, pre-filled with the current text.
3. Starts a query-replace from the beginning of the buffer.
4. For each occurrence, you can press:
   - 'y' to replace this occurrence.
   - 'n' to skip this occurrence.
   - '!' to replace all remaining occurrences without asking.
   - 'q' to exit the query-replace.

Returns to the original point after completing (or erroring)."
  (interactive "P")
  (let ((text-to-replace
         (if (use-region-p)
             ;; If region is active, use the selected text.
             (buffer-substring-no-properties (region-beginning) (region-end))
           ;; Otherwise use the symbol at point.
           (let ((bounds (bounds-of-thing-at-point 'symbol)))
             (if bounds
                 (buffer-substring-no-properties (car bounds) (cdr bounds))
               (error "davidc-rename-symbol-in-buffer: No symbol at point and no region selected."))))))
    ;; Call the core function with no region boundaries (nil means use whole buffer).
    (davidc--rename-symbol-core text-to-replace nil nil no-whole-symbol-only)))

(defun davidc--rename-symbol-in-region (start end &optional no-whole-symbol-only)
  "Rename occurrences of a symbol, but only within the specified region.
Useful for renaming a variable within a single function or block.

START and END define the region to operate on.
With a prefix argument, matches fragments inside larger symbols as well.

The function will:
1. Identify the symbol at point or prompt for a symbol if none is found
2. Prompt for a replacement string, pre-filled with the original text
3. Perform interactive query-replace only within the selected region
4. Return to the original cursor position after completion

For each occurrence, you can press:
   - 'y' to replace this occurrence
   - 'n' to skip this occurrence
   - '!' to replace all remaining occurrences without asking
   - 'q' to exit the query-replace"
  (interactive "r\nP")
  (let* (;; Get symbol at point, or prompt for one if none is found.
         (symbol-at-point (thing-at-point 'symbol t))
         (text-to-replace
          (if symbol-at-point
              symbol-at-point
            (davidc--read-string "Symbol to replace: "))))
    ;; Call the core function with the specified region boundaries.
    (davidc--rename-symbol-core text-to-replace start end no-whole-symbol-only)))

(defun davidc-rename-symbol-in-function (&optional no-whole-symbol-only)
  "Rename occurrences of a symbol, but only within the current function/defun.
With a prefix argument, matches fragments inside larger symbols as well.

The function identifies the current defun boundaries and then calls
`davidc--rename-symbol-in-region` to perform the actual renaming operation.

If no function boundaries can be identified, falls back to either:
1. The current active region, if one exists.
2. The entire buffer with symbol at point, if no region is selected.

For each occurrence, you can press:
   - 'y' to replace this occurrence
   - 'n' to skip this occurrence
   - '!' to replace all remaining occurrences without asking
   - 'q' to exit the query-replace"
  (interactive "P")
  (let* ((bounds (bounds-of-thing-at-point 'defun))
         (defun-start (if bounds (car bounds) (point-min)))
         (defun-end (if bounds (cdr bounds) (point-max))))

    (if bounds
        (progn
          (message "Renaming within current function/defun...")
          (davidc--rename-symbol-in-region defun-start defun-end no-whole-symbol-only))
      (message "Could not identify current function/defun boundaries. Using current region or selecting symbol at point.")
      (if (use-region-p)
          (davidc--rename-symbol-in-region (region-beginning) (region-end) no-whole-symbol-only)
        (call-interactively 'davidc-rename-symbol-in-buffer)))))


(defvar davidc--expand-region-history nil
  "History of region expansions for grow/shrink region functions.")

(defun davidc-region-grow ()
  "Increase the selected region by syntactic units.
If there is no active region, select the nearest symbol, word, or sexp.
If on whitespace with nothing nearby, selects the current line.
Subsequent calls expand to progressively larger syntactic units:
- Nearest symbol or word
- Surrounding whitespace
- Expression/sexp containing point
- Parent expression/sexp
- Current defun/function
- Entire buffer"
  (interactive)
  ;; Initialize history as buffer-local if needed.
  (unless (local-variable-p 'davidc--expand-region-history)
    (setq-local davidc--expand-region-history nil))

  (if (not (region-active-p))
      ;; No active region - find something to start with.
      (let ((symbol-bounds (bounds-of-thing-at-point 'symbol))
            (word-bounds (bounds-of-thing-at-point 'word))
            (sexp-bounds nil))

        ;; Try to get sexp bounds safely.
        (condition-case nil
            (save-excursion
              (let ((sexp-start (progn (backward-sexp) (point)))
                    (sexp-end (progn (forward-sexp) (point))))
                (setq sexp-bounds (cons sexp-start sexp-end))))
          (error nil))

        ;; Find the most appropriate thing to select.
        (cond
         ;; First try symbol (usually best for code).
         (symbol-bounds
          (setq davidc--expand-region-history nil) ; Reset history.
          (push symbol-bounds davidc--expand-region-history)
          (goto-char (cdr symbol-bounds))
          (set-mark (car symbol-bounds)))

         ;; If no symbol, try word.
         (word-bounds
          (setq davidc--expand-region-history nil)
          (push word-bounds davidc--expand-region-history)
          (goto-char (cdr word-bounds))
          (set-mark (car word-bounds)))

         ;; If no word, try sexp nearby.
         (sexp-bounds
          (setq davidc--expand-region-history nil)
          (push sexp-bounds davidc--expand-region-history)
          (goto-char (cdr sexp-bounds))
          (set-mark (car sexp-bounds)))

         ;; If nothing found, select the current line.
         (t
          (setq davidc--expand-region-history nil)
          (let ((line-bounds (bounds-of-thing-at-point 'line)))
            (if line-bounds
                (progn
                  (push line-bounds davidc--expand-region-history)
                  (goto-char (cdr line-bounds))
                  (set-mark (car line-bounds)))
              ;; Absolute fallback - select current line by positions.
              (beginning-of-line)
              (set-mark (point))
              (end-of-line)
              (push (cons (mark) (point)) davidc--expand-region-history)))))

        (activate-mark)
        (setq deactivate-mark nil))

    ;; We have an active region - try to expand it.
    (let ((current-start (region-beginning))
          (current-end (region-end))
          (expanded nil))

      ;; First try to expand to a larger containing sexp.
      (save-excursion
        (goto-char current-start)
        (condition-case nil
            (progn
              (backward-up-list)
              (let ((sexp-start (point)))
                (forward-sexp)
                (let ((sexp-end (point)))
                  (when (and (< sexp-start current-start)
                             (> sexp-end current-end))
                    ;; We found a larger sexp.
                    (push (cons sexp-start sexp-end) davidc--expand-region-history)
                    (setq expanded t)
                    ;; Store the new bounds to use them later.
                    (setq current-start sexp-start)
                    (setq current-end sexp-end)))))
          (error nil)))

      ;; If we couldn't expand to a sexp, try the current defun.
      (when (not expanded)
        (save-excursion
          (goto-char current-start)
          (condition-case nil
              (let ((defun-start (progn (beginning-of-defun) (point)))
                    (defun-end (progn (end-of-defun) (point))))
                (when (and (< defun-start current-start)
                           (> defun-end current-end))
                  (push (cons defun-start defun-end) davidc--expand-region-history)
                  (setq expanded t)
                  (setq current-start defun-start)
                  (setq current-end defun-end)))
            (error nil))))

      ;; If we still couldn't expand, try paragraph.
      (when (not expanded)
        (save-excursion
          (goto-char current-start)
          (condition-case nil
              (let ((para-start (progn (backward-paragraph) (point)))
                    (para-end (progn (forward-paragraph) (point))))
                (when (and (< para-start current-start)
                           (> para-end current-end))
                  (push (cons para-start para-end) davidc--expand-region-history)
                  (setq expanded t)
                  (setq current-start para-start)
                  (setq current-end para-end)))
            (error nil))))

      ;; If we couldn't expand to a paragraph or we're already at one, expand to buffer.
      (when (not expanded)
        (let ((buffer-bounds (cons (point-min) (point-max))))
          (push buffer-bounds davidc--expand-region-history)
          (setq current-start (car buffer-bounds))
          (setq current-end (cdr buffer-bounds))))

      ;; Update the region with the new bounds.
      (goto-char current-end)
      (set-mark current-start)
      (activate-mark)
      (setq deactivate-mark nil))))

(defun davidc-region-shrink ()
  "Decrease the selected region by syntactic units.
This is the inverse operation of `davidc-region-grow`.
It steps back through the expansion history."
  (interactive)
  (when (and (local-variable-p 'davidc--expand-region-history)
             davidc--expand-region-history
             (> (length davidc--expand-region-history) 1))
    ;; Remove the current (largest) region.
    (pop davidc--expand-region-history)
    ;; Get the previous region.
    (let ((prev-region (car davidc--expand-region-history)))
      (goto-char (cdr prev-region))
      (set-mark (car prev-region))
      (activate-mark)
      (setq deactivate-mark nil))))


(defun davidc-unfill-paragraph (&optional region)
  "Reverse the effect of `fill-paragraph' by joining all lines in the current paragraph into a single line.
If REGION is non-nil, operate on the active region instead."
  (interactive (list (use-region-p)))
  (let ((fill-column most-positive-fixnum)) ; Set fill-column to a very large value
    (if region
        (fill-region (region-beginning) (region-end))
      (fill-paragraph nil))))

(defun davidc-unfill-region (start end)
  "Unfill all paragraphs in the region from START to END."
  (interactive "r")  ; "r" means to use the region as arguments
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

(defun davidc-unfill-buffer ()
  "Unfill all paragraphs in the current buffer, converting them into single lines."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-region (point-min) (point-max))))

;; Highlighting Lines
;;
;; This example shows you how to make lines containing the words
;; “ERROR:” or “NOTE:” highlighted, whenever a file ending in “log” is
;; opened.
(defun davidc-highlight-it ()
  "Highlight certain lines."
  (interactive)
  (if (equal "log" (file-name-extension (buffer-file-name)))
    (progn
      (highlight-lines-matching-regexp "ERROR:" 'hi-red-b)
      (highlight-lines-matching-regexp "WARN:" 'hi-yellow-b)
      (highlight-lines-matching-regexp "WARNING:" 'hi-yellow-b)
      (highlight-lines-matching-regexp "NOTE:" 'hi-green-b)
      )
    (progn
      (highlight-phrase "ERROR:" 'hi-red-b)
      (highlight-phrase "TODO:" 'hi-green-b)
      (highlight-phrase ".. todo::" 'hi-green-b)
      (highlight-phrase "NOTE:" 'hi-green-b)
      (highlight-phrase ".. note::" 'hi-green-b)
      )
    ))


(defun davidc-fullscreen (&optional f)
  "Toggle Fullscreen mode."
  (interactive)
  (set-frame-parameter f 'fullscreen
                       (if (frame-parameter f 'fullscreen) nil 'fullboth)))


;; Hide Dired details by default.
;;
;; http://xahlee.info/emacs/emacs/emacs_dired_tips.html
(defun davidc-dired-mode-details-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))


(defun davidc-format-region-c++ ()
  "Format a region of C++ code.
For now we use 'clang-format'."
  (interactive)
  (call-interactively 'clang-format-region))


(defun davidc-format-buffer-c++ ()
  "Format a C++ buffer.
For now we use 'clang-format'."
  (interactive)
  (call-interactively 'clang-format-buffer))


;; https://emacs.stackexchange.com/questions/12148/how-to-pretty-format-code-auto-insert-newlines-indent-etc
(defun davidc-format-region-json ()
  "Formats a region of JSON."
  (interactive)
  (save-excursion
    (shell-command-on-region (region-beginning)
                             (region-end)
                             "python -m json.tool"
                             (buffer-name)
                             t)))


(defun davidc-format-region-python ()
  "Formats a region of Python code.
For now we use 'python black'."
  (interactive)
  (call-interactively 'python-black-region))


(defun davidc-format-buffer-python ()
  "Formats a Python buffer.
For now we use 'python black'."
  (interactive)
  (call-interactively 'python-black-buffer))

(defun davidc-format-buffer-rust ()
  "Format a Rust buffer.
For now we use 'rustfmt'."
  (interactive)
  (call-interactively 'rust-format-buffer))

(defun davidc-format ()
  "Format the buffer or region for supported major modes.
Supported major modes are C++ (c++-mode) and Python (python-mode)."
  (interactive)
  ;; http://xahlee.info/emacs/emacs/emacs_region.html
  (if (use-region-p)
      (call-interactively 'davidc-format-region)
    (call-interactively 'davidc-format-buffer)))

(defun davidc-format-region ()
  "Format the selected region of text for supported major modes.
Supported major modes are C++ (c++-mode) and Python (python-mode).
For Rust (rust-mode), we format the entire buffer instead."
  (interactive)
  (cond
   ((string-equal major-mode "c++-mode") (call-interactively 'davidc-format-region-c++))
   ((string-equal major-mode "python-mode") (call-interactively 'davidc-format-region-python))
   ((string-equal major-mode "rust-mode")
    (message "Region formatting not supported for Rust - formatting entire buffer")
    (call-interactively 'davidc-format-buffer-rust))
   ))

(defun davidc-format-buffer ()
  "Format the selected buffer of text for supported major modes.
Supported major modes are C++ (c++-mode), Python (python-mode) and Rust (rust-mode)."
  (interactive)
  (cond
   ((string-equal major-mode "c++-mode") (call-interactively 'davidc-format-buffer-c++))
   ((string-equal major-mode "python-mode") (call-interactively 'davidc-format-buffer-python))
   ((string-equal major-mode "rust-mode") (call-interactively 'davidc-format-buffer-rust))
   )
  )

;; https://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
;;
;; This function assumes the user has a region active (ie. that both
;; mark-active and transient-mark-mode are non-nil).
(defun davidc-move-region-internal (line-offset-number)
   ;; Makes the point (cursor location) go to the top of the active
   ;; (selection) region.
   (if (> (point) (mark))
       (exchange-point-and-mark))

   ;; Ensure the region uses contents of the first line, so the line
   ;; is not chopped off.
   (move-to-column 0 t)

   ;; Move active (selection) region up/down
   (let ((text (delete-and-extract-region (point) (mark))))
     (forward-line line-offset-number)
     (set-mark (point))
     (insert text)
     (exchange-point-and-mark)
     (setq deactivate-mark nil))
   )


(defun davidc-move-region-down (line-offset-number)
  "Move region (transient-mark-mode active)
  line-offset-number lines down."
  (interactive "*p")
  (davidc-move-region-internal line-offset-number))


(defun davidc-move-region-up (line-offset-number)
  "Move region (transient-mark-mode active)
  line-offset-number lines up."
  (interactive "*p")
  (davidc-move-region-internal (- line-offset-number)))


;; https://emacs.stackexchange.com/questions/13941/move-selected-lines-up-and-down
(defun davidc-move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))


(defun davidc-move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))


(defun davidc-move-line-or-region-up ()
  "Move up the active region, or the current line."
  (interactive)
  (if (and mark-active transient-mark-mode)
      (call-interactively 'davidc-move-region-up)
    (call-interactively 'davidc-move-line-up)))


(defun davidc-move-line-or-region-down ()
  "Move down the active region, or the current line."
  (interactive)
  (if (and mark-active transient-mark-mode)
      (call-interactively 'davidc-move-region-down)
    (call-interactively 'davidc-move-line-down)))


;; As the built-in project.el support expects to use vc-mode hooks to
;; find the root of projects we need to provide something equivalent
;; for it.
;;
;; https://www.reddit.com/r/emacs/comments/nf2k5y/comment/gyjs516/?utm_source=share&utm_medium=web2x&context=3
(defun davidc-git-project-finder (dir)
  "Integrate .git project roots."
  (let ((dotgit (and (setq dir (locate-dominating-file dir ".git"))
                     (expand-file-name dir))))
    (and dotgit
         (cons 'transient (file-name-directory dotgit)))))


(defun davidc-string-inflection-toggle-function (str)
  "Not so much the case that in all caps when using normal foo_bar <--> fooBar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-lower-camelcase-function str))
   ((string-inflection-lower-camelcase-p str)
    (string-inflection-underscore-function str))
   (t
    (string-inflection-underscore-function str))))

(defun davidc-string-inflection-toggle ()
  "Toggle foo_bar <=> fooBar"
  (interactive)
  (string-inflection-insert
   (davidc-string-inflection-toggle-function (string-inflection-get-current-word))))

(defun davidc-string-inflection-cycle-auto ()
  "Switching case cylcing by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))

   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))

   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))

   ;; for elixir
   ((eq major-mode 'elixir-mode)
    (string-inflection-elixir-style-cycle))

   ;; default
   (t
    (string-inflection-python-style-cycle)))
  )


;; Code folding cycling.
;;
;; https://karthinks.com/software/simple-folding-with-hideshow/
;;
;; Note: This code looks like it has some redundant clauses you can
;; refactor using hs-already-hidden-p, and like you don't need to set
;; last-command for all the clauses. Don’t try this, it breaks in
;; subtle ways.
(defun davidc-hs-cycle (&optional level)
  (interactive "p")
  (let (message-log-max
        (inhibit-message t))
    (if (= level 1)
        (pcase last-command
          ('davidc-hs-cycle
           (hs-hide-level 1)
           (setq this-command 'hs-cycle-children))
          ('hs-cycle-children
           ;; TODO: Fix this case. `hs-show-block' needs to be
           ;; called twice to open all folds of the parent
           ;; block.
           (save-excursion (hs-show-block))
           (hs-show-block)
           (setq this-command 'hs-cycle-subtree))
          ('hs-cycle-subtree
           (hs-hide-block))
          (_
           (if (not (hs-already-hidden-p))
               (hs-hide-block)
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level))))

;; Toggles the hide-show cycle for the entire buffer.
(defun davidc-hs-global-cycle ()
  (interactive)
  (pcase last-command
    ('davidc-hs-global-cycle
     (save-excursion (hs-show-all))
     (setq this-command 'hs-global-show))
    (_ (hs-hide-all))))


;; Display how many line numbers a folded area shows.
;;
;; From the documentation, but modified a little bit.
(defun davidc-hs-display-code-line-counts (ov)
   (when (eq 'code (overlay-get ov 'hs))
     (overlay-put ov 'display
                  (format "... [%d lines]"
                          (count-lines (overlay-start ov)
                                       (overlay-end ov))))))

;; Try the following to display the overlay content in a tooltip.
;;
;; https://www.emacswiki.org/emacs/HideShow#h5o-7
(defun davidc-hs-display-code-as-tooltip (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'help-echo
                 (buffer-substring (overlay-start ov)
                                   (overlay-end ov)))))

;; This is intended for debugging the emacs configuration. It can be
;; run to restart Emacs in a fresh state.
(defun davidc-debug-restart-emacs ()
  "Restart Emacs completely for debugging purposes."
  (interactive)
  (let ((init-file user-init-file)
        (restart-args command-line-args))

    ;; Start a new Emacs process
    (call-process (concat invocation-directory invocation-name)
                  nil 0 nil
                  "--eval" "(message \"Restarted Emacs for debugging.\")"
                  "--file" init-file)

    ;; Exit this Emacs process
    (kill-emacs)))

;; This is intended for debugging the emacs configuration. It can be
;; run to restart Emacs in a fresh state.
(defun davidc-debug-restart-emacs-preserve-buffers ()
  "Restart Emacs completely for debugging purposes while preserving open buffers."
  (interactive)
  (let ((init-file user-init-file)
        (buf-names (mapcar 'buffer-file-name
                           (seq-filter 'buffer-file-name (buffer-list))))
        (current-buf (buffer-file-name))
        (desktop-file (expand-file-name "emacs-restart-buffers.el" temporary-file-directory)))

    ;; Save buffer list to temporary file
    (with-temp-file desktop-file
      (insert "(progn\n")
      ;; Reopen all files.
      (dolist (file buf-names)
        (when file
          (insert (format "  (find-file \"%s\")\n" file))))
      ;; Return to current buffer.
      (when current-buf
        (insert (format "  (find-file \"%s\")\n" current-buf)))
      (insert "  (message \"Emacs restarted with %d buffers restored.\"))\n"
              (length buf-names)))

    ;; Start a new Emacs process.
    (call-process (concat invocation-directory invocation-name)
                  nil 0 nil
                  "--eval" (format "(load \"%s\")" desktop-file)
                  "--file" init-file)

    ;; Exit this Emacs process.
    (kill-emacs)))


;; Define default settings if not already defined in custom-vars.el
(defvar davidc-python-flymake-ruff-path
  (cond
   ((string-equal system-type "windows-nt") ; Windows
    "ruff.exe")
   ((string-equal system-type "darwin") ; macOS
    "ruff")
   (t ; Linux or other
    "ruff")))

;; Setup flymake configuration for Python with ruff.
(defun davidc-python-flymake-ruff-setup ()
  "Configure flymake for Python using ruff."
  (when (and (boundp 'python-flymake-command)
             (boundp 'python-flymake-command-output-pattern)
             (boundp 'python-flymake-msg-alist))
    ;; Use the settings from custom-vars.el if they exist
    ;; Otherwise, use default values
    (unless (symbol-value 'python-flymake-command)
      (setq-local python-flymake-command
                  (list davidc-python-flymake-ruff-path "check"
                        "--stdin-filename=<stdin>"
                        "-")))

    (unless (symbol-value 'python-flymake-command-output-pattern)
      (setq-local python-flymake-command-output-pattern
                  '("^\\(?:<?stdin>?\\):\\(?1:[0-9]+\\):\\(?:\\(?2:[0-9]+\\):\\)? \\(?3:.*\\)$" 1 2 nil 3)))

    (unless (symbol-value 'python-flymake-msg-alist)
      (setq-local python-flymake-msg-alist
                  '(("\\(^redefinition\\|.*unused.*\\|used$\\)" . :warning)
                    ("^E999" . :error)
                    ("^[EW][0-9]+" . :note))))))

;; Define default settings for clang-tidy path.
(defvar davidc-flymake-clang-tidy-path
  (cond
   ((string-equal system-type "windows-nt") ; Windows
    "clang-tidy.exe")
   ((string-equal system-type "darwin") ; macOS
    "clang-tidy")
   (t ; Linux or other
    "clang-tidy"))
  "Path to the clang-tidy executable.")

;; Local variable to keep track of the currently running flymake
;; clang-tidy process.
(defvar-local davidc--flymake-clang-tidy-proc nil
  "Current clang-tidy flymake process.")

(defun davidc-flymake-clang-tidy-on-save ()
  "Run Flymake only when buffer is saved."
  (when flymake-mode
    (flymake-start)))

;; Add Clang-tidy as a backend for Flymake.
;;
;; This uses this annotated example for ruby as a starting point:
;; https://www.gnu.org/software/emacs/manual/html_node/flymake/An-annotated-example-backend.html
(defun davidc-flymake-clang-tidy (report-fn &rest _args)
  "Flymake backend for clang-tidy.
REPORT-FN is the callback function for reporting diagnostics."
  ;; Check if clang-tidy exists.
  (unless (executable-find davidc-flymake-clang-tidy-path)
    (error "Flymake mode clang-tidy; Cannot find clang-tidy executable at \"%s\"." davidc-flymake-clang-tidy-path))
  ;; (message "[DEBUG] Flymake mode clang-tidy; using clang-tidy executable at \"%s\"." davidc-flymake-clang-tidy-path)

  ;; Kill any existing process.
  (when (process-live-p davidc--flymake-clang-tidy-proc)
    (kill-process davidc--flymake-clang-tidy-proc))

  ;; Save the current buffer.
  (let* ((source (current-buffer))
         (source-file (buffer-file-name)))

    (when source-file
      ;; Save buffer before running clang-tidy to ensure all changes are analyzed.
      (when (buffer-modified-p)
        (error "Flymake mode clang-tidy; buffer is not saved, cannot check it."))

      ;; (message "[DEBUG] Flymake mode clang-tidy; Source File: \"%s\"." source-file)
      (save-restriction
        (widen)

        ;; Reset the process variable and create a new process with
        ;; improved options.
        (setq davidc--flymake-clang-tidy-proc
              (make-process
               :name "flymake-clang-tidy"
               :noquery t
               :connection-type 'pipe
               :buffer (generate-new-buffer " *flymake-clang-tidy*")
               ;; ':file-handler' will ensure the current working
               ;; directory is set to the current buffer's value of
               ;; 'default-directory', or '~' as a fallback.
               :file-handler t
               :command (list davidc-flymake-clang-tidy-path
                              "-quiet"
                              (file-name-nondirectory source-file)
                              ;; NOTE: Passing this "--" seems to call
                              ;; the compiler directly. On Windows
                              ;; that is not possible, so we don't get
                              ;; much value from enabling this.
                              ;;
                              ;; "--"
                              )
               :sentinel
               (lambda (proc _event)
                 ;; Check that the process has indeed exited.
                 (when (memq (process-status proc) '(exit signal))
                   (unwind-protect
                       ;; Only proceed if proc is the current process.
                       (if (with-current-buffer source (eq proc davidc--flymake-clang-tidy-proc))
                           (with-current-buffer (process-buffer proc)
                             ;; ;; Debug output - print the entire buffer content
                             ;; (let ((output-content (buffer-string)))
                             ;;   (message "[DEBUG] clang-tidy output: \n%s" output-content))

                             (goto-char (point-min))
                             (let ((diags '()))
                               ;; Update regex to capture the file path as well
                               (while (search-forward-regexp
                                       "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(warning\\|error\\|note\\): \\(.*\\)$"
                                       nil t)
                                 (let* ((full-match (match-string 0))
                                        (file-path (match-string 1))
                                        (line (string-to-number (match-string 2)))
                                        (col (string-to-number (match-string 3)))
                                        (type (match-string 4))
                                        (msg (match-string 5))
                                        (level (cond
                                                ((string= type "error") :error)
                                                ((string= type "warning") :warning)
                                                (t :note)))

                                        ;; Get region for diagnostics.
                                        (beg-end (flymake-diag-region
                                                 source
                                                 line
                                                 col)))

                                   ;; ;; Debug output for each match
                                   ;; (message "[DEBUG] Match: %s" full-match)
                                   ;; (message "[DEBUG] File: %s" file-path)
                                   ;; (message "[DEBUG] Current file: %s" source-file)
                                   ;; (message "[DEBUG] Line: %d, Col: %d, Type: %s" line col type)
                                   ;; (message "[DEBUG] Message: %s" msg)
                                   ;; (message "[DEBUG] Level: %s" level)

                                   ;; Check if this diagnostic is for the current file
                                   ;; Try to match by comparing normalized paths
                                   (let* ((normalized-source (expand-file-name source-file))
                                          (normalized-file (expand-file-name (directory-file-name file-path)))
                                          (is-current-file (or
                                                            (string= normalized-file normalized-source)
                                                            (string-suffix-p (file-name-nondirectory source-file) file-path))))

                                     ;; (message "[DEBUG] Is current file: %s" is-current-file)

                                     ;; Only add diagnostics for the current file, and ignore notes.
                                     (when (and is-current-file
                                                beg-end
                                                (not (eq level :note))) ; Skip note-level diagnostics
                                       ;; (message "[DEBUG] Adding diagnostic for current file")
                                       ;; (message "[DEBUG] Region: %s-%s" (car beg-end) (cdr beg-end))
                                       (push (flymake-make-diagnostic source
                                                                    (car beg-end)
                                                                    (cdr beg-end)
                                                                    level
                                                                    msg)
                                             diags)))))

                               ;; Report the diagnostics.
                               (message "Flymake mode clang-tidy; Found %d diagnostics for \"%s\"."
                                       (length diags)
                                       (file-name-nondirectory source-file))
                               (funcall report-fn diags)))

                         ;; If obsolete, log warning.
                         (flymake-log :warning "Canceling obsolete check %s" proc))

                     ;; Clean up resources.
                     (when (process-buffer proc)
                       (kill-buffer (process-buffer proc))))))))))))

(defun davidc-flymake-clang-tidy-setup ()
  "Set up flymake for clang-tidy."
  (interactive)
  (message "Setting up clang-tidy for flymake.")
  ;; Add our clang-tidy backend to the list of diagnostic functions.
  (add-hook 'flymake-diagnostic-functions #'davidc-flymake-clang-tidy nil t)
  ;; Enable flymake.
  (flymake-mode 1))


;; Define default settings for Rust's Cargo executable path.
(defvar davidc-flymake-rust-cargo-path
  (cond
   ((string-equal system-type "windows-nt") ; Windows
    "cargo.exe")
   ((string-equal system-type "darwin") ; macOS
    "cargo")
   (t ; Linux or other
    "cargo"))
  "Path to the Rust cargo executable.")

(defvar davidc-flymake-rust-cargo-clippy-args
  '()
  "Arguments to pass to cargo clippy.
This should be a list of strings, each element being a separate argument.
By default, no extra arguments are passed, using Clippy's default settings.

Some common options include:
* (\"-W\" \"clippy::all\") - Enable all clippy warnings
* (\"-W\" \"clippy::pedantic\") - Enable pedantic clippy warnings
* (\"-A\" \"clippy::some_lint\") - Disable a specific lint

See `cargo clippy --help` for more options.")

;; Local variable to keep track of the currently running flymake
;; rust-cargo-clippy process.
(defvar-local davidc--flymake-rust-cargo-clippy-proc nil
  "Current cargo clippy flymake process.")

(defun davidc-flymake-rust-cargo-clippy-on-save ()
  "Run Flymake only when buffer is saved."
  (when flymake-mode
    (flymake-start)))

(defun davidc-flymake-rust-cargo-clippy (report-fn &rest _args)
  "Flymake backend for Rust's clippy.
REPORT-FN is the callback function for reporting diagnostics."
  ;; Check if rust-cargo-clippy exists.
  (unless (executable-find davidc-flymake-rust-cargo-path)
    (error "Flymake mode rust-cargo-clippy; Cannot find cargo executable at \"%s\"." davidc-flymake-rust-cargo-path))
  ;; (message "[DEBUG] Flymake mode rust-cargo-clippy; using cargo executable at \"%s\"." davidc-flymake-rust-cargo-path)

  ;; Kill any existing process.
  (when (process-live-p davidc--flymake-rust-cargo-clippy-proc)
    (kill-process davidc--flymake-rust-cargo-clippy-proc))

  ;; Save the current buffer.
  (let* ((source (current-buffer))
         (source-file (buffer-file-name)))

    (when source-file
      ;; Ensure buffer is saved
      (when (buffer-modified-p)
        (error "Flymake mode rust-cargo-clippy; buffer is not saved, cannot check it."))

      ;; (message "[DEBUG] Flymake mode rust-cargo-clippy; Source File: \"%s\"." source-file)
      (save-restriction
        (widen)

        ;; Get the path components for the current file for later comparison
        (let* ((source-dir (file-name-directory source-file))
               (source-basename (file-name-nondirectory source-file))
               (proj-dir (locate-dominating-file source-file "Cargo.toml")))

          ;; If we can't find a Cargo.toml, warn the user
          (unless proj-dir
            (message "Warning: No Cargo.toml found in parent directories of %s" source-file))

          ;; Create the process
          (setq davidc--flymake-rust-cargo-clippy-proc
                (make-process
                 :name "flymake-rust-cargo-clippy"
                 :noquery t
                 :connection-type 'pipe
                 :buffer (generate-new-buffer " *flymake-rust-cargo-clippy*")
                 ;; ':file-handler' will ensure the current working
                 ;; directory is set to the current buffer's value of
                 ;; 'default-directory', or '~' as a fallback.
                 :file-handler t
                 :command (append (list davidc-flymake-rust-cargo-path
                                        "clippy"
                                        "--message-format=short"
                                        "--")
                                  davidc-flymake-rust-cargo-clippy-args)
                 :sentinel
                 (lambda (proc _event)
                   ;; Check that the process has indeed exited.
                   (when (memq (process-status proc) '(exit signal))
                     (unwind-protect
                         ;; Only proceed if proc is the current process.
                         (if (with-current-buffer source (eq proc davidc--flymake-rust-cargo-clippy-proc))
                             (with-current-buffer (process-buffer proc)
                               ;; ;; Debug output - print the entire buffer content
                               ;; (let ((output-content (buffer-string)))
                               ;;   (message "[DEBUG] rust-cargo-clippy full output:\n%s" output-content))

                               (goto-char (point-min))
                               (let ((diags '()))

                                 ;; Parse the Rust error/warning lines directly
                                 ;; Format: file:line:col: warning/error: message
                                 (while (re-search-forward "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(warning\\|error\\(?:\\[E[0-9]+\\]\\)?\\): \\(.*\\)$" nil t)
                                   (let* ((file-path (match-string 1))
                                          (line (string-to-number (match-string 2)))
                                          (col (string-to-number (match-string 3)))
                                          (type-str (match-string 4))
                                          (message-text (match-string 5))
                                          (level (if (string-match-p "^error" type-str) :error :warning))
                                          (file-basename (file-name-nondirectory file-path))
                                          (is-current-file (string= file-basename source-basename)))

                                     ;; (message "[DEBUG] Found: %s at %s:%d:%d - %s"
                                     ;;          type-str file-path line col message-text)
                                     ;; (message "[DEBUG] Current file: %s, Parsed file: %s, Match: %s"
                                     ;;          source-basename file-basename is-current-file)

                                     ;; Check if this is for the current file by comparing basenames
                                     (when is-current-file
                                       (let ((beg-end (flymake-diag-region source line col)))
                                         (when beg-end
                                           ;; (message "[DEBUG] Adding diagnostic: %s at %d:%d"
                                           ;;          message-text line col)
                                           (push (flymake-make-diagnostic source
                                                                          (car beg-end)
                                                                          (cdr beg-end)
                                                                          level
                                                                          message-text)
                                                 diags))))))

                                 ;; Report the diagnostics.
                                 (message "Flymake mode rust-cargo-clippy; Found %d diagnostics for \"%s\"."
                                          (length diags)
                                          (file-name-nondirectory source-file))
                                 (funcall report-fn diags)))

                           ;; If obsolete, log warning.
                           (flymake-log :warning "Canceling obsolete check %s" proc))

                       ;; Clean up resources.
                       (when (process-buffer proc)
                         (kill-buffer (process-buffer proc)))))))))))))

(defun davidc-flymake-rust-cargo-clippy-setup ()
  "Set up flymake for rust clippy."
  (interactive)
  (message "Setting up Rust clippy for flymake.")
  ;; Add our rust clippy backend to the list of diagnostic functions.
  (add-hook 'flymake-diagnostic-functions #'davidc-flymake-rust-cargo-clippy nil t)
  ;; Enable flymake.
  (flymake-mode 1))


(defun davidc-flymake-get-sorted-diagnostics (reverse)
  "Get all Flymake diagnostics sorted by position.
If REVERSE is non-nil, sort in reverse order."
  (let ((diagnostics (flymake-diagnostics)))
    (sort diagnostics
          (lambda (a b)
            (if reverse
                (> (flymake-diagnostic-beg a) (flymake-diagnostic-beg b))
              (< (flymake-diagnostic-beg a) (flymake-diagnostic-beg b)))))))

(defun davidc-flymake-diagnostic-matches-p (diag level current-line current-point direction wrapped)
  "Check if diagnostic DIAG matches the search criteria.
LEVEL is the diagnostic level to match (:error, :warning, or :note).
CURRENT-LINE and CURRENT-POINT define the current position.
DIRECTION can be 'next or 'prev to determine search direction.
WRAPPED if non-nil means we're searching after wrapping around the buffer."
  (and
   ;; Check position based on direction and wrapped state.
   (if wrapped
       ;; When wrapped, we're searching the entire buffer.
       t
     ;; When not wrapped, respect direction.
     (if (eq direction 'next)
         ;; For next: find diagnostics after current point.
         (or (> (line-number-at-pos (flymake-diagnostic-beg diag)) current-line)
             (and (= (line-number-at-pos (flymake-diagnostic-beg diag)) current-line)
                  (> (flymake-diagnostic-beg diag) current-point)))
       ;; For prev: find diagnostics before current point.
       (or (< (line-number-at-pos (flymake-diagnostic-beg diag)) current-line)
           (and (= (line-number-at-pos (flymake-diagnostic-beg diag)) current-line)
                (< (flymake-diagnostic-beg diag) current-point)))))
   ;; Check for exact type match.
   (eq (flymake-diagnostic-type diag) level)))

(defun davidc-flymake-goto-diagnostic (direction &optional level)
  "Go to the next/previous Flymake diagnostic with specified LEVEL.
Will wrap around the buffer if no matching diagnostic is found.

DIRECTION can be 'next or 'prev.
LEVEL can be :error, :warning, or :note. If not specified, use :error."
  (interactive)
  ;; Check if flymake is running in the current buffer.
  (if (not flymake-mode)
      (message "Flymake not running in current buffer")

    (let* ((level-value (or level :error))
           (current-line (line-number-at-pos))
           (current-point (point))
           (diagnostics (davidc-flymake-get-sorted-diagnostics (eq direction 'prev)))
           (found nil)
           (wrapped nil)
           (level-name (cond
                        ((eq level-value :error) "error")
                        ((eq level-value :warning) "warning")
                        ((eq level-value :note) "note"))))

      ;; If there are no diagnostics at all, inform the user and exit early
      (if (null diagnostics)
          (message "No diagnostics found in buffer")

    ;; First attempt: search in specified direction without wrapping.
    (dolist (diag diagnostics)
      (when (and (not found)
                 (davidc-flymake-diagnostic-matches-p
                  diag level-value current-line current-point direction wrapped))
        (goto-char (flymake-diagnostic-beg diag))
        (setq found t)))

    ;; Second attempt: if not found, search again with wrapping.
    (unless found
      (setq wrapped t)
      ;; For wrapping, consider all diagnostics of the specified type,
      ;; regardless of their position relative to point.
      (dolist (diag diagnostics)
        (when (and (not found)
                   (davidc-flymake-diagnostic-matches-p
                    diag level-value current-line current-point direction wrapped))
          (goto-char (flymake-diagnostic-beg diag))
          (setq found t)))

      ;; If we found one on the wrap, show a more descriptive message.
      (when found
        (message "Reached %s of buffer, wrapped to %s and found %s %s"
                 (if (eq direction 'next) "end" "beginning")
                 (if (eq direction 'next) "beginning" "end")
                 (if (eq level-value :error) "an" "a")
                 level-name)))

    ;; Show message if nothing found at all.
    (unless found
      (message "No %s diagnostics found in buffer" level-name))))))

;; Define convenience functions.
(defun flymake-next-error ()
  "Go to the next Flymake error.
The search will wrap around the buffer if needed."
  (interactive)
  (davidc-flymake-goto-diagnostic 'next :error))

(defun flymake-prev-error ()
  "Go to the previous Flymake error.
The search will wrap around the buffer if needed."
  (interactive)
  (davidc-flymake-goto-diagnostic 'prev :error))

(defun flymake-next-warning ()
  "Go to the next Flymake warning.
The search will wrap around the buffer if needed."
  (interactive)
  (davidc-flymake-goto-diagnostic 'next :warning))

(defun flymake-prev-warning ()
  "Go to the previous Flymake warning.
The search will wrap around the buffer if needed."
  (interactive)
  (davidc-flymake-goto-diagnostic 'prev :warning))

(defun flymake-next-note ()
  "Go to the next Flymake note.
The search will wrap around the buffer if needed."
  (interactive)
  (davidc-flymake-goto-diagnostic 'next :note))

(defun flymake-prev-note ()
  "Go to the previous Flymake note.
The search will wrap around the buffer if needed."
  (interactive)
  (davidc-flymake-goto-diagnostic 'prev :note))
