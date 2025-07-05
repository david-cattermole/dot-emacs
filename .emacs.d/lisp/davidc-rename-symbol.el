;;; -*- lexical-binding: t -*-
;;
;; Rename Symbol under cursor.
;;
;; You can rename the symbol on the current line, current function or
;; current buffer.
;;

(defun davidc--read-string (prompt initial-input)
  "Read a string from the minibuffer, with delete-selection-mode enabled."
  ;; Save and restore delete-selection-mode state to avoid side effects.
  (let ((old-delete-selection-mode delete-selection-mode)
        (result nil))
    (minibuffer-with-setup-hook
        (lambda ()
          ;; If you enable Delete Selection (minor) mode, then inserting
          ;; text while the mark is active causes the selected text to
          ;; be deleted first. This also deactivates the mark. Many
          ;; graphical applications follow this convention, but Emacs
          ;; does not.
          (delete-selection-mode 1))
      (unwind-protect
          (setq result (read-string prompt initial-input))
        ;; Restore the original state of delete-selection-mode.
        (delete-selection-mode (if old-delete-selection-mode 1 -1))))
    result))

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
            (davidc--read-string "Symbol to replace: " ""))))
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

(provide 'davidc-rename-symbol)
