;;; -*- lexical-binding: t -*-
;;
;; Region tools.
;;
;; Grow and shrink the current selection.
;;

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

(provide 'davidc-region)
