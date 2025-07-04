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

(defun davidc-rename-symbol-in-line (&optional no-whole-symbol-only)
  "Rename occurrences of a symbol, but only within the current line.
With a prefix argument, matches fragments inside larger symbols as well.

The function identifies the current line boundaries and then performs
symbol renaming only within that line. This is useful for quick renames
of variables that appear multiple times on the same line.

If no symbol is found at point, prompts for a symbol to replace.

For each occurrence, you can press:
   - 'y' to replace this occurrence
   - 'n' to skip this occurrence
   - '!' to replace all remaining occurrences without asking
   - 'q' to exit the query-replace"
  (interactive "P")
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         ;; Get symbol at point, or prompt for one if none is found.
         (symbol-at-point (thing-at-point 'symbol t))
         (text-to-replace
          (if symbol-at-point
              symbol-at-point
            (davidc--read-string "Symbol to replace: " ""))))

    (message "Renaming within current line...")
    ;; Call the core function with the current line boundaries.
    (davidc--rename-symbol-core text-to-replace line-start line-end no-whole-symbol-only)))


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
  (if (version< emacs-version "25.0")
      (progn
        (set-frame-parameter f 'fullscreen
                             (if (frame-parameter f 'fullscreen) nil 'fullboth)))
    (progn
      ;; 'toggle-frame-fullscreen' was added in Emacs 24.4.
      (toggle-frame-fullscreen))))


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


;; Find Perforce projects.
;;
;; Similar to the 'davidc-git-project-finder', but for Perforce (p4).
(defun davidc-perforce-project-finder (dir)
  "Integrate .p4rc project roots."
  (let ((dotgit (and (setq dir (locate-dominating-file dir ".p4rc"))
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


;; Helper function to save current buffer state for restoration after
;; restart.
(defun davidc--save-buffer-list-for-restart (desktop-file message-suffix)
  "Save current buffer list to DESKTOP-FILE for restoration after restart.
MESSAGE-SUFFIX will be appended to the restoration message."
  (let ((buf-names (mapcar 'buffer-file-name
                           (seq-filter 'buffer-file-name (buffer-list))))
        (current-buf (buffer-file-name)))

    ;; Save buffer list to temporary file.
    (with-temp-file desktop-file
      (insert "(progn\n")
      ;; Reopen all files.
      (dolist (file buf-names)
        (when file
          (insert (format "  (find-file \"%s\")\n" file))))
      ;; Return to current buffer.
      (when current-buf
        (insert (format "  (find-file \"%s\")\n" current-buf)))
      (insert (format "  (message \"Emacs restarted%s and %%d buffers restored.\"))\n"
                       message-suffix)
              (length buf-names)))

    ;; Return the desktop file path for use by caller.
    desktop-file))

;; This is intended for debugging the emacs configuration. It can be
;; run to restart Emacs in a fresh state, but with all buffers
;; re-opened.
(defun davidc-restart-emacs ()
  "Restart Emacs completely for debugging purposes while preserving open buffers."
  (interactive)
  (let ((init-file user-init-file)
        (desktop-file (expand-file-name "emacs-restart-buffers.el" temporary-file-directory)))

    ;; Save buffer list to temporary file.
    (davidc--save-buffer-list-for-restart desktop-file "")

    ;; Start a new Emacs process.
    (call-process (concat invocation-directory invocation-name)
                  nil 0 nil
                  "--eval" (format "(load \"%s\")" desktop-file)
                  "--file" init-file)

    ;; Exit this Emacs process.
    (kill-emacs)))

;; Restart Emacs with '--debug-init' flag for debugging configuration
;; issues.
(defun davidc-restart-emacs-debug-init ()
  "Restart Emacs with --debug-init for debugging configuration while preserving open buffers."
  (interactive)
  (let ((init-file user-init-file)
        (desktop-file (expand-file-name "emacs-restart-buffers.el" temporary-file-directory)))

    ;; Save buffer list to temporary file.
    (davidc--save-buffer-list-for-restart desktop-file " with debug-init")

    ;; Start a new Emacs process with debug-init.
    (call-process (concat invocation-directory invocation-name)
                  nil 0 nil
                  "--debug-init"
                  "--eval" (format "(load \"%s\")" desktop-file)
                  "--file" init-file)

    ;; Exit this Emacs process.
    (kill-emacs)))

;; Restart Emacs with '--no-init-file' flag for debugging without
;; configuration.
(defun davidc-restart-emacs-no-init ()
  "Restart Emacs with --no-init-file (vanilla Emacs) while preserving open buffers."
  (interactive)
  (let ((desktop-file (expand-file-name "emacs-restart-buffers.el" temporary-file-directory)))

    ;; Save buffer list to temporary file
    (davidc--save-buffer-list-for-restart desktop-file " with no-init-file")

    ;; Start a new Emacs process without init file.
    (call-process (concat invocation-directory invocation-name)
                  nil 0 nil
                  "--no-init-file"
                  "--eval" (format "(load \"%s\")" desktop-file))

    ;; Exit this Emacs process.
    (kill-emacs)))


(defun davidc-symbol-highlight-or-dired-details-toggle ()
  "Smart toggle function for symbol-highlight or dired-details.

In dired buffers: toggle dired-hide-details-mode.
In other buffers: toggle symbol highlight lock."
  (interactive)
  (cond
   ;; In dired mode, toggle details.
   ((derived-mode-p 'dired-mode)
    (if (fboundp 'dired-hide-details-mode)
        ;; Toggle `ls -1` (dash one) and `ls -l` (dash lower case L)
        ;; output in Dried.
        (dired-hide-details-mode 'toggle)
      (message "dired-hide-details-mode not available")))

   ;; In other buffers, toggle symbol highlight lock.
   (t
    (if (bound-and-true-p davidc-symbol-highlight-mode)
        (davidc-symbol-highlight-toggle-symbol-lock)
      ;; If symbol highlight mode is not active, enable it first.
      (progn
        (davidc-symbol-highlight-mode 1)
        (message "Enabled symbol highlighting mode"))))))
