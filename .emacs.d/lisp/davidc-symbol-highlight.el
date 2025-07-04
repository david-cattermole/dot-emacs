;;; -*- lexical-binding: t -*-
;;
;; Symbol Highlighting.
;;
;; This Emacs package enables dynamically highlighting the active
;; symbol, when multiple occurances of that symbol are visible in the
;; file.
;;
;; Enable symbol highlighting by enabling the global minor-mode:
;;    (davidc-global-symbol-highlight-mode 1)
;; Or enable just the minor-mode just in the current buffer:
;;    (davidc-symbol-highlight-mode 1)
;;
;; Move the cursor to the next occurance of the symbol:
;;    M-x davidc-symbol-highlight-next-occurrence
;; Or to the previous occurance:
;;    M-x davidc-symbol-highlight-prev-occurrence
;;
;; You may optionally "lock" the symbol under the cursor with:
;;    M-x davidc-symbol-highlight-toggle-symbol-lock
;;
;; To view the number of symbol highlights, you can run:
;;    M-x davidc-symbol-highlight-show-count
;;
;; Or you can look at the Emacs mode-line 'SymH[#]' is
;; the number of occurances.
;;

(defgroup davidc-symbol-highlight nil
  "Highlight symbols under cursor."
  :group 'davidc-config)

(defface davidc-symbol-highlight-face
  '((t (:background "DarkSlateGray4" :foreground "white")))
  "Face for highlighting symbols under cursor."
  :group 'davidc-symbol-highlight)

(defface davidc-symbol-highlight-locked-face
  '((t (:background "DarkSlateGray4" :foreground "white" :weight bold)))
  "Face for locked symbol highlighting."
  :group 'davidc-symbol-highlight)

(defcustom davidc-symbol-highlight-delay 0.3
  "Delay in seconds before highlighting symbols after cursor movement."
  :type 'number
  :group 'davidc-symbol-highlight)

(defcustom davidc-symbol-highlight-minimum-length 1
  "Minimum length of symbol to highlight."
  :type 'integer
  :group 'davidc-symbol-highlight)

(defcustom davidc-symbol-highlight-include-numbers t
  "When non-nil, highlight numeric symbols like '42'."
  :type 'boolean
  :group 'davidc-symbol-highlight)

;; Buffer-local variables.
(defvar-local davidc--symbol-highlight-overlays nil
  "List of overlays for current symbol highlighting.")

(defvar-local davidc--symbol-highlight-locked nil
  "When non-nil, contains the locked symbol string.")

(defvar-local davidc--symbol-highlight-timer nil
  "Timer for delayed symbol highlighting.")

(defvar-local davidc--symbol-highlight-count nil
  "Number of occurrences of the highlighted symbol.")

(defun davidc--symbol-highlight-cleanup ()
  "Remove all symbol highlight overlays."
  (when davidc--symbol-highlight-overlays
    (mapc #'delete-overlay davidc--symbol-highlight-overlays)
    (setq davidc--symbol-highlight-overlays nil))
  (setq davidc--symbol-highlight-count 0))

(defun davidc--symbol-highlight-get-symbol ()
  "Get the symbol at point, or nil if invalid."
  (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
              (symbol (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (when (and (>= (length symbol) davidc-symbol-highlight-minimum-length)
                   ;; Optionally exclude pure numbers.
                   (or davidc-symbol-highlight-include-numbers
                       (not (string-match-p "^[0-9]+$" symbol))))
      symbol)))

(defun davidc--symbol-highlight-create-overlays (symbol face)
  "Create overlays for all occurrences of SYMBOL using FACE.
This includes occurrences in invisible/hidden text."
  (save-excursion
    (goto-char (point-min))
    (let ((overlays '())
          (case-fold-search nil)
          (regex (concat "\\_<" (regexp-quote symbol) "\\_>"))
          ;; Ensure we search invisible text.
          (search-invisible t))
      (while (re-search-forward regex nil t)
        (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put ov 'face face)
          (overlay-put ov 'davidc-symbol-highlight t)
          ;; Make overlay visible even in hidden text.
          (overlay-put ov 'priority 1000)
          (push ov overlays)))
      overlays)))

(defun davidc--symbol-highlight-update ()
  "Update symbol highlighting based on current context."
  ;; Cancel any pending timer.
  (when davidc--symbol-highlight-timer
    (cancel-timer davidc--symbol-highlight-timer)
    (setq davidc--symbol-highlight-timer nil))

  ;; Clean up existing overlays.
  (davidc--symbol-highlight-cleanup)

  ;; If we have a locked symbol, highlight it.
  (if davidc--symbol-highlight-locked
      (let ((overlays (davidc--symbol-highlight-create-overlays
                       davidc--symbol-highlight-locked
                       'davidc-symbol-highlight-locked-face)))
        ;; Only set overlays if there are 2 or more occurrences.
        (if (>= (length overlays) 2)
            (progn
              (setq davidc--symbol-highlight-overlays overlays)
              (setq davidc--symbol-highlight-count (length overlays)))
          ;; Clean up if less than 2 occurrences.
          (mapc #'delete-overlay overlays)
          (setq davidc--symbol-highlight-count 0)))
    ;; Otherwise, highlight symbol at point.
    (when-let ((symbol (davidc--symbol-highlight-get-symbol)))
      (let ((overlays (davidc--symbol-highlight-create-overlays
                       symbol
                       'davidc-symbol-highlight-face)))
        ;; Only set overlays if there are 2 or more occurrences.
        (if (>= (length overlays) 2)
            (progn
              (setq davidc--symbol-highlight-overlays overlays)
              (setq davidc--symbol-highlight-count (length overlays)))
          ;; Clean up if less than 2 occurrences.
          (mapc #'delete-overlay overlays)
          (setq davidc--symbol-highlight-count 0))))))

(defun davidc--symbol-highlight-post-command ()
  "Post-command hook for symbol highlighting."
  (when (and davidc-symbol-highlight-mode
             (not davidc--symbol-highlight-locked)  ; Don't update if locked.
             (not (minibufferp))
             (not (region-active-p)))
    ;; Schedule update with delay.
    (when davidc--symbol-highlight-timer
      (cancel-timer davidc--symbol-highlight-timer))
    (setq davidc--symbol-highlight-timer
          (run-with-timer davidc-symbol-highlight-delay nil
                          #'davidc--symbol-highlight-update))))

(defun davidc-symbol-highlight-toggle-symbol-lock ()
  "Toggle symbol locking.
If unlocked: lock the symbol at point.
If locked: unlock and return to auto-highlighting."
  (interactive)
  (unless davidc-symbol-highlight-mode
    (user-error "Symbol highlighting is not enabled"))

  (if davidc--symbol-highlight-locked
      ;; Currently locked - unlock it.
      (progn
        (setq davidc--symbol-highlight-locked nil)
        (davidc--symbol-highlight-update)
        (message "Symbol highlighting unlocked - auto-highlighting enabled"))
    ;; Currently unlocked - lock current symbol.
    (if-let ((symbol (davidc--symbol-highlight-get-symbol)))
        (progn
          (setq davidc--symbol-highlight-locked symbol)
          (davidc--symbol-highlight-update)
          (if (> davidc--symbol-highlight-count 0)
              (message "Locked symbol '%s' (%d occurrences)" symbol davidc--symbol-highlight-count)
            (message "Locked symbol '%s' (no other occurrences)" symbol)))
      (message "No symbol at point to lock"))))

;;;###autoload
(define-minor-mode davidc-symbol-highlight-mode
  "Minor mode for highlighting symbols under cursor.
When enabled, automatically highlights all occurrences of the symbol
at point. Use `davidc-symbol-highlight-toggle-symbol-lock' to lock/unlock
highlighting to a specific symbol."
  :init-value nil
  :lighter (:eval (davidc--symbol-highlight-mode-line))
  (if davidc-symbol-highlight-mode
      (progn
        (add-hook 'post-command-hook #'davidc--symbol-highlight-post-command nil t)
        (davidc--symbol-highlight-update))
    ;; Cleanup when disabling.
    (remove-hook 'post-command-hook #'davidc--symbol-highlight-post-command t)
    (when davidc--symbol-highlight-timer
      (cancel-timer davidc--symbol-highlight-timer)
      (setq davidc--symbol-highlight-timer nil))
    (setq davidc--symbol-highlight-locked nil)
    (davidc--symbol-highlight-cleanup)))

(defun davidc--symbol-highlight-mode-line ()
  "Generate the mode-line string for symbol highlighting."
  (if (> davidc--symbol-highlight-count 0)
      (format " SymH[%s%d]"
              (if davidc--symbol-highlight-locked "L" "")
              davidc--symbol-highlight-count)
    (if davidc--symbol-highlight-locked " SymH[L]" " SymH")))

;;;###autoload
(define-globalized-minor-mode davidc-global-symbol-highlight-mode
  davidc-symbol-highlight-mode
  (lambda ()
    (when (and (not (minibufferp))
               (derived-mode-p 'prog-mode 'text-mode))
      (davidc-symbol-highlight-mode 1))))

;; Cleanup hook for buffer/mode changes.
(add-hook 'change-major-mode-hook
          (lambda ()
            (when (bound-and-true-p davidc-symbol-highlight-mode)
              (davidc-symbol-highlight-mode -1))))

(defun davidc--symbol-highlight-reveal-at-point ()
  "Reveal hidden text at point and ensure point is visible.
Handles various hiding mechanisms including org-mode, hideshow, and outline-mode.
Returns t if something was revealed, nil otherwise."
  (let ((revealed nil))
    (cond
     ;; Handle org-mode folded sections.
     ((and (derived-mode-p 'org-mode)
           (org-invisible-p))
      (org-show-context 'link-search)
      (setq revealed t))

     ;; Handle hideshow hidden blocks.
     ((and (bound-and-true-p hs-minor-mode)
           (hs-already-hidden-p))
      (hs-show-block)
      (setq revealed t))

     ;; Handle outline-mode and outline-minor-mode.
     ((and (or (derived-mode-p 'outline-mode)
               (bound-and-true-p outline-minor-mode))
           (outline-invisible-p))
      (outline-show-entry)
      (setq revealed t))

     ;; Handle general invisible text property.
     ((get-text-property (point) 'invisible)
      ;; Try to find the beginning of the invisible region and make it
      ;; visible.
      (let ((start (point))
            (end (point)))
        ;; Find start of invisible region.
        (while (and (> start (point-min))
                    (get-text-property (1- start) 'invisible))
          (setq start (1- start)))
        ;; Find end of invisible region.
        (while (and (< end (point-max))
                    (get-text-property end 'invisible))
          (setq end (1+ end)))
        ;; Remove invisible property from the region.
        (remove-text-properties start end '(invisible nil))
        (setq revealed t))))

    ;; Ensure the point is visible in the window.
    (when revealed
      ;; Force redisplay to update the buffer.
      (redisplay t)
      ;; Recenter if necessary to make sure the point is visible.
      (unless (pos-visible-in-window-p)
        (recenter)))

    revealed))

(defun davidc--symbol-highlight-find-current-overlay ()
  "Find the symbol highlight overlay at current position.
Returns the overlay if found, nil otherwise."
  (let ((overlays (overlays-at (point)))
        (found nil))
    (dolist (ov overlays)
      (when (and (not found)
                 (overlay-get ov 'davidc-symbol-highlight))
        (setq found ov)))
    found))

(defun davidc-symbol-highlight-next-occurrence ()
  "Jump to the next occurrence of the highlighted symbol.
Wraps around to the beginning of the buffer if no occurrence is found after point.
Reveals the occurrence if it's hidden."
  (interactive)
  (unless (and davidc-symbol-highlight-mode davidc--symbol-highlight-overlays)
    (user-error "No highlighted symbol"))

  (let* ((current-pos (point))
         (overlays (sort (copy-sequence davidc--symbol-highlight-overlays)
                        (lambda (a b) (< (overlay-start a) (overlay-start b)))))
         (next-overlay nil)
         (wrapped nil))

    ;; Find the first overlay that starts after current position.
    (dolist (ov overlays)
      (when (and (not next-overlay) (> (overlay-start ov) current-pos))
        (setq next-overlay ov)))

    ;; If no overlay found after current position, wrap to first.
    (unless next-overlay
      (setq next-overlay (car overlays))
      (setq wrapped t))

    (when next-overlay
      (let ((target-pos (overlay-start next-overlay)))
        ;; Move to the target position.
        (goto-char target-pos)

        ;; Reveal hidden text at the new position.
        (davidc--symbol-highlight-reveal-at-point)

        ;; Ensure we're still at the right position after revealing
        ;; (revealing might have changed buffer positions).
        (goto-char target-pos)

        ;; Make sure the position is visible in the window.
        (unless (pos-visible-in-window-p)
          (recenter))

        (when wrapped
          (message "Wrapped to beginning"))))))

(defun davidc-symbol-highlight-prev-occurrence ()
  "Jump to the previous occurrence of the highlighted symbol.
Wraps around to the end of the buffer if no occurrence is found before point.
Reveals the occurrence if it's hidden."
  (interactive)
  (unless (and davidc-symbol-highlight-mode davidc--symbol-highlight-overlays)
    (user-error "No highlighted symbol"))

  (let* ((current-pos (point))
         ;; Check if we're inside a current overlay.
         (current-overlay (davidc--symbol-highlight-find-current-overlay))
         ;; If inside an overlay, use its start position as reference.
         (reference-pos (if current-overlay
                           (overlay-start current-overlay)
                         current-pos))
         (overlays (sort (copy-sequence davidc--symbol-highlight-overlays)
                        (lambda (a b) (> (overlay-start a) (overlay-start b)))))
         (prev-overlay nil)
         (wrapped nil))

    ;; Find the first overlay that starts before reference position.
    (dolist (ov overlays)
      (when (and (not prev-overlay)
                 (< (overlay-start ov) reference-pos)
                 ;; Exclude the current overlay from consideration.
                 (not (eq ov current-overlay)))
        (setq prev-overlay ov)))

    ;; If no overlay found before current position, wrap to last.
    (unless prev-overlay
      (setq prev-overlay (car overlays))
      (setq wrapped t))

    (when prev-overlay
      (let ((target-pos (overlay-start prev-overlay)))
        ;; Move to the target position.
        (goto-char target-pos)

        ;; Reveal hidden text at the new position.
        (davidc--symbol-highlight-reveal-at-point)

        ;; Ensure we're still at the right position after revealing
        ;; (revealing might have changed buffer positions).
        (goto-char target-pos)

        ;; Make sure the position is visible in the window.
        (unless (pos-visible-in-window-p)
          (recenter))

        (when wrapped
          (message "Wrapped to end"))))))

(defun davidc-symbol-highlight-show-count ()
  "Display the count of highlighted symbol occurrences."
  (if (and davidc-symbol-highlight-mode davidc--symbol-highlight-overlays)
  (interactive)
      (let ((symbol-name (or davidc--symbol-highlight-locked
                            (davidc--symbol-highlight-get-symbol)
                            "symbol")))
        (message "%s'%s': %d occurrences"
                 (if davidc--symbol-highlight-locked "[LOCKED] " "")
                 symbol-name
                 (length davidc--symbol-highlight-overlays)))
    (message "No highlighted symbol")))
