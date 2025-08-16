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
  '((t (:inverse-video t)))
  "Face for highlighting symbols under cursor."
  :group 'davidc-symbol-highlight)

(defface davidc-symbol-highlight-locked-face
  '((t (:weight bold :slant italic :inverse-video t)))
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

(defcustom davidc-symbol-highlight-case-sensitive t
  "When non-nil, symbol highlighting is case sensitive.
When nil, symbols are highlighted regardless of case.
This is the default setting that applies unless overridden by
`davidc-symbol-highlight-case-sensitive-modes'."
  :type 'boolean
  :group 'davidc-symbol-highlight)

(defcustom davidc-symbol-highlight-case-sensitive-modes
  '((emacs-lisp-mode . t)
     (c-mode . t)
     (c++-mode . t)
     (python-mode . t)
     (rust-mode . t)
     (js-mode . t)
     (js-json-mode . t)
     (sh-mode . t)
     (conf-mode . t)
     (sql-mode . nil)
     (bat-mode . nil)
     (fortran-mode . nil))
  "Alist of major modes and their case sensitivity settings.
Each element is (MODE . CASE-SENSITIVE-P) where MODE is a major mode
symbol and CASE-SENSITIVE-P is t for case sensitive or nil for case
insensitive. If a mode is not in this list, the value of
`davidc-symbol-highlight-case-sensitive' is used."
  :type '(alist :key-type symbol :value-type boolean)
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

(defvar-local davidc--symbol-highlight-opened-overlays nil
  "List of overlays that have been opened for symbol highlighting.
Similar to isearch-opened-overlays, tracks overlays we've made visible.")

(defvar-local davidc--symbol-highlight-navigating nil
  "When t, indicates we're in the middle of navigation.
Used to prevent post-command hook from interfering with opened overlays.")

(defun davidc--symbol-highlight-case-sensitive-p ()
  "Return whether symbol highlighting should be case sensitive in current buffer.
Checks `davidc-symbol-highlight-case-sensitive-modes' first, then falls
back to `davidc-symbol-highlight-case-sensitive'."
  (let ((mode-setting (assoc major-mode davidc-symbol-highlight-case-sensitive-modes)))
    (if mode-setting
        (cdr mode-setting)
      davidc-symbol-highlight-case-sensitive)))

(defun davidc--symbol-highlight-cleanup ()
  "Remove all symbol highlight overlays and restore hidden overlays."
  (when davidc--symbol-highlight-overlays
    (mapc #'delete-overlay davidc--symbol-highlight-overlays)
    (setq davidc--symbol-highlight-overlays nil))
  ;; Only close opened overlays if we're not currently navigating.
  ;;
  ;; This prevents revealed text from being hidden immediately after
  ;; navigation.
  (unless davidc--symbol-highlight-navigating
    (when davidc--symbol-highlight-opened-overlays
      (mapc #'davidc--symbol-highlight-close-overlay davidc--symbol-highlight-opened-overlays)
      (setq davidc--symbol-highlight-opened-overlays nil)))
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
This includes occurrences in invisible/hidden text.
Respects the case sensitivity setting for the current mode."
  (save-excursion
    (goto-char (point-min))
    (let ((overlays '())
          (case-fold-search (not (davidc--symbol-highlight-case-sensitive-p)))
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
             (not davidc--symbol-highlight-navigating)  ; Don't update during navigation.
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

;; Cleanup hooks for buffer/mode changes.
(add-hook 'change-major-mode-hook
          (lambda ()
            (when (bound-and-true-p davidc-symbol-highlight-mode)
              (davidc-symbol-highlight-mode -1))))

(add-hook 'kill-buffer-hook
          (lambda ()
            (when (bound-and-true-p davidc-symbol-highlight-mode)
              (davidc--symbol-highlight-cleanup))))

(defun davidc--symbol-highlight-open-overlay-temporary (ov)
  "Open overlay OV temporarily for symbol highlighting.
Based on isearch-open-overlay-temporary. Only opens overlays that can be opened
according to isearch conventions (have isearch-open-invisible property)."
  (when (overlay-get ov 'invisible)
    (if (not (null (overlay-get ov 'isearch-open-invisible-temporary)))
        ;; If overlay has custom temporary opening function, use it.
        (funcall (overlay-get ov 'isearch-open-invisible-temporary) ov nil)
      ;; Store the original invisible property and make overlay
      ;; visible.
      (overlay-put ov 'davidc-symbol-highlight-invisible (overlay-get ov 'invisible))
      (overlay-put ov 'invisible nil))))

(defun davidc--symbol-highlight-close-overlay (ov)
  "Close overlay OV by restoring its original invisible property."
  (let ((fct-temp (overlay-get ov 'isearch-open-invisible-temporary)))
    (if fct-temp
        ;; If overlay has custom temporary function, use it to close
        (funcall fct-temp ov t)
      ;; Restore original invisible property
      (overlay-put ov 'invisible (overlay-get ov 'davidc-symbol-highlight-invisible))
      (overlay-put ov 'davidc-symbol-highlight-invisible nil))))

(defun davidc--symbol-highlight-intersects-p (start0 end0 start1 end1)
  "Return t if regions START0..END0 and START1..END1 intersect."
  (or (and (>= start0 start1) (<  start0 end1))
      (and (>  end0 start1)   (<= end0 end1))
      (and (>= start1 start0) (<  start1 end0))
      (and (>  end1 start0)   (<= end1 end0))))

(defun davidc--symbol-highlight-close-unnecessary-overlays (beg end)
  "Close overlays in opened-overlays that don't intersect BEG to END region.
Based on isearch-close-unnecessary-overlays."
  (let ((overlays davidc--symbol-highlight-opened-overlays))
    (setq davidc--symbol-highlight-opened-overlays nil)
    (dolist (ov overlays)
      (if (davidc--symbol-highlight-intersects-p beg end (overlay-start ov) (overlay-end ov))
          ;; Keep overlay open if it intersects current region.
          (push ov davidc--symbol-highlight-opened-overlays)
        ;; Close overlay if it doesn't intersect.
        (davidc--symbol-highlight-close-overlay ov)))))

(defun davidc--symbol-highlight-range-invisible-p (beg end)
  "Return t if all text from BEG to END is invisible.
Based on isearch-range-invisible but simplified for symbol highlighting."
  (when (/= beg end)
    (save-excursion
      (goto-char beg)
      ;; Check if any character in the range is invisible.
      (catch 'visible
        (while (< (point) end)
          (unless (invisible-p (point))
            (throw 'visible nil))
          (goto-char (1+ (point))))
        t))))

(defun davidc--symbol-highlight-reveal-range (beg end)
  "Reveal overlays in range BEG to END that can be opened.
Based on isearch logic. Returns t if any overlays were opened."
  (when (/= beg end)
    (save-excursion
      (goto-char beg)
      (let ((can-be-opened t)
            (crt-overlays nil))
        ;; Check invisibility across the range.
        (while (and (< (point) end) (invisible-p (point)))
          (if (invisible-p (get-text-property (point) 'invisible))
              ;; Text property invisibility - skip and cannot be opened.
              (progn
                (goto-char (next-single-property-change (point) 'invisible nil end))
                (setq can-be-opened nil))
            ;; Overlay invisibility - check if openable.
            (when can-be-opened
              (let ((overlays (overlays-at (point)))
                    ov-list o invis-prop)
                (while overlays
                  (setq o (car overlays)
                        invis-prop (overlay-get o 'invisible))
                  (when (invisible-p invis-prop)
                    (if (overlay-get o 'isearch-open-invisible)
                        ;; Can be opened.
                        (push o ov-list)
                      ;; Cannot be opened - whole chunk fails.
                      (setq can-be-opened nil)))
                  (setq overlays (cdr overlays)))
                (when can-be-opened
                  (setq crt-overlays (append ov-list crt-overlays)))))
            (goto-char (next-overlay-change (point)))))

        ;; Open collected overlays if we can.
        (when (and can-be-opened (consp crt-overlays))
          (setq davidc--symbol-highlight-opened-overlays
                (append davidc--symbol-highlight-opened-overlays crt-overlays))
          (mapc #'davidc--symbol-highlight-open-overlay-temporary crt-overlays)
          t))))

(defun davidc--symbol-highlight-reveal-overlays-at-point ()
  "Reveal overlays with invisible property at current point.
Returns t if something was revealed, nil otherwise."
  (let ((overlays (overlays-at (point)))
        (revealed nil))
    (dolist (ov overlays)
      (when (and (overlay-get ov 'invisible)
                 (overlay-get ov 'isearch-open-invisible))
        ;; Open overlay temporarily and track it.
        (davidc--symbol-highlight-open-overlay-temporary ov)
        (push ov davidc--symbol-highlight-opened-overlays)
        (setq revealed t)))
    revealed))

(defun davidc--symbol-highlight-reveal-at-point ()
  "Reveal hidden text at point and ensure point is visible.
Uses a hybrid approach: overlay-based for overlays, fallback for other mechanisms.
Returns t if something was revealed, nil otherwise."
  (let ((revealed nil))
    ;; First try overlay-based approach.
    (when (davidc--symbol-highlight-reveal-overlays-at-point)
      (setq revealed t))

    ;; Fallback to mode-specific mechanisms for non-overlay hiding.
    (cond
     ;; Handle org-mode folded sections (org uses overlays but also
     ;; text properties).
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
      (setq revealed t)))

    ;; Ensure the point is visible in the window.
    (when revealed
      ;; Force redisplay to update the buffer.
      (redisplay t)
      ;; Recenter if necessary to make sure the point is visible.
      (unless (pos-visible-in-window-p)
        (recenter)))

    revealed)))

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

  ;; Set navigation flag to prevent post-command hook interference.
  (setq davidc--symbol-highlight-navigating t)
  ;; Cancel any pending timer that might interfere.
  (when davidc--symbol-highlight-timer
    (cancel-timer davidc--symbol-highlight-timer)
    (setq davidc--symbol-highlight-timer nil))

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
      (let ((target-pos (overlay-start next-overlay))
            (target-end (overlay-end next-overlay)))
        ;; Close any previously opened overlays that don't intersect
        ;; with target.
        (davidc--symbol-highlight-close-unnecessary-overlays target-pos target-end)

        ;; Move to the target position first.
        (goto-char target-pos)

        ;; Reveal hidden text if the target symbol is invisible.
        (when (davidc--symbol-highlight-range-invisible-p target-pos target-end)
          (or (davidc--symbol-highlight-reveal-range target-pos target-end)
              (davidc--symbol-highlight-reveal-at-point)))

        ;; Ensure we're still at the right position after revealing.
        (goto-char target-pos)

        ;; Make sure the position is visible in the window.
        (unless (pos-visible-in-window-p)
          (recenter))

        (when wrapped
          (message "Wrapped to beginning"))))

    ;; Clear navigation flag after a short delay to allow post-command hooks to run.
    (run-with-idle-timer 0.1 nil (lambda () (setq davidc--symbol-highlight-navigating nil)))))

(defun davidc-symbol-highlight-prev-occurrence ()
  "Jump to the previous occurrence of the highlighted symbol.
Wraps around to the end of the buffer if no occurrence is found before point.
Reveals the occurrence if it's hidden."
  (interactive)
  (unless (and davidc-symbol-highlight-mode davidc--symbol-highlight-overlays)
    (user-error "No highlighted symbol"))

  ;; Set navigation flag to prevent post-command hook interference.
  (setq davidc--symbol-highlight-navigating t)
  ;; Cancel any pending timer that might interfere.
  (when davidc--symbol-highlight-timer
    (cancel-timer davidc--symbol-highlight-timer)
    (setq davidc--symbol-highlight-timer nil))

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
      (let ((target-pos (overlay-start prev-overlay))
            (target-end (overlay-end prev-overlay)))
        ;; Close any previously opened overlays that don't intersect
        ;; with target.
        (davidc--symbol-highlight-close-unnecessary-overlays target-pos target-end)

        ;; Move to the target position first.
        (goto-char target-pos)

        ;; Reveal hidden text if the target symbol is invisible.
        (when (davidc--symbol-highlight-range-invisible-p target-pos target-end)
          (or (davidc--symbol-highlight-reveal-range target-pos target-end)
              (davidc--symbol-highlight-reveal-at-point)))

        ;; Ensure we're still at the right position after revealing
        ;; (revealing might have changed buffer positions).
        (goto-char target-pos)

        ;; Make sure the position is visible in the window.
        (unless (pos-visible-in-window-p)
          (recenter))

        (when wrapped
          (message "Wrapped to end"))))

    ;; Clear navigation flag after a short delay to allow post-command
    ;; hooks to run.
    (run-with-idle-timer 0.1 nil (lambda () (setq davidc--symbol-highlight-navigating nil)))))

(defun davidc-symbol-highlight-show-count ()
  "Display the count of highlighted symbol occurrences."
  (interactive)
  (if (and davidc-symbol-highlight-mode davidc--symbol-highlight-overlays)
      (let ((symbol-name (or davidc--symbol-highlight-locked
                            (davidc--symbol-highlight-get-symbol)
                            "symbol")))
        (message "%s'%s': %d occurrences"
                 (if davidc--symbol-highlight-locked "[LOCKED] " "")
                 symbol-name
                 (length davidc--symbol-highlight-overlays)))
    (message "No highlighted symbol")))

(provide 'davidc-symbol-highlight)
