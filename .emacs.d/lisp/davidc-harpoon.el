;;; -*- lexical-binding: t -*-
;;
;; Harpoon-inspired quick navigation system for Emacs.
;;
;; This package provides a streamlined way to mark and quickly jump
;; between important locations in your code, inspired by NeoVim's
;; Harpoon plugin.
;;
;; FEATURES:
;; - Mark up to 9 specific positions in files.
;; - Quick jump with single keystroke (1-9).
;; - Visual menu showing all marked positions.
;; - LRU-based cycling through marks.
;; - Support for file positions and dired directories.
;; - Persistent marks across Emacs sessions (optional).
;; - Standard Emacs buffer for menu (searchable with C-s).
;; - Individual mark clearing and consolidation (remove gaps/sort).
;; - Multiple deletion options (individual, confirmed, clear all).
;;
;; BASIC USAGE:
;; Mark current position: M-x davidc-harpoon-mark-position
;; Jump to mark 1-9: M-x davidc-harpoon-jump-to-1 (through 9)
;; Show menu: M-x davidc-harpoon-menu
;; Cycle marks: M-x davidc-harpoon-next / davidc-harpoon-prev
;;
;; SUGGESTED KEY BINDINGS:
;; (global-set-key (kbd "C-c h m") 'davidc-harpoon-mark-position)
;; (global-set-key (kbd "C-c h l") 'davidc-harpoon-menu)
;; (global-set-key (kbd "C-<f3>") 'davidc-harpoon-prev)
;; (global-set-key (kbd "C-<f4>") 'davidc-harpoon-next)
;; (global-set-key (kbd "C-c h c") 'davidc-harpoon-clear-marks)
;;
;; ;; Quick jumps with Super key (Windows key on PC)
;; (global-set-key (kbd "s-1") 'davidc-harpoon-jump-to-1)
;; (global-set-key (kbd "s-2") 'davidc-harpoon-jump-to-2)
;; ;; ... etc for 3-9
;;
;; ;; Or with Alt key
;; (global-set-key (kbd "M-1") 'davidc-harpoon-jump-to-1)
;; (global-set-key (kbd "M-2") 'davidc-harpoon-jump-to-2)
;; ;; ... etc for 3-9

(require 'cl-lib)
(require 'dired)

(defgroup davidc-harpoon nil
  "Quick navigation system inspired by Harpoon."
  :group 'convenience
  :prefix "davidc-harpoon-")

(defcustom davidc-harpoon-max-marks 9
  "Maximum number of marks to maintain."
  :type 'integer
  :group 'davidc-harpoon)

(defcustom davidc-harpoon-persist-marks nil
  "Whether to persist marks across Emacs sessions."
  :type 'boolean
  :group 'davidc-harpoon)

(defcustom davidc-harpoon-save-file
  (expand-file-name "harpoon-marks.el" user-emacs-directory)
  "File to save persistent marks."
  :type 'file
  :group 'davidc-harpoon)

(defcustom davidc-harpoon-show-preview t
  "Whether to show preview of marked locations in menu.
Only applies to position-specific marks."
  :type 'boolean
  :group 'davidc-harpoon)

(defcustom davidc-harpoon-preview-length 50
  "Maximum length of preview text in menu."
  :type 'integer
  :group 'davidc-harpoon)

(defface davidc-harpoon-mark-number
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for mark numbers in harpoon menu."
  :group 'davidc-harpoon)

(defface davidc-harpoon-buffer-name
  '((t (:inherit font-lock-keyword-face)))
  "Face for buffer names in harpoon menu."
  :group 'davidc-harpoon)

(defface davidc-harpoon-line-number
  '((t (:inherit font-lock-constant-face)))
  "Face for line numbers in harpoon menu."
  :group 'davidc-harpoon)

(defface davidc-harpoon-preview
  '((t (:inherit font-lock-comment-face)))
  "Face for preview text in harpoon menu."
  :group 'davidc-harpoon)

(defface davidc-harpoon-directory
  '((t (:inherit dired-directory)))
  "Face for directory marks in harpoon menu."
  :group 'davidc-harpoon)

(defface davidc-harpoon-buffer-only
  '((t (:inherit font-lock-type-face)))
  "Face for buffer-only mark indicator."
  :group 'davidc-harpoon)

;; Internal variables.
(defvar davidc-harpoon--marks nil
  "List of harpoon marks.
Each mark is a plist with keys:
  :buffer - buffer or buffer name
  :file - file path (if applicable)
  :position - position in buffer (optional)
  :line - line number (optional)
  :column - column number (optional)
  :preview - preview text (optional)
  :type - 'file or 'dired
  :position-saved - t if position was explicitly saved
  :timestamp - last access time for LRU")

(defvar davidc-harpoon--menu-buffer-name "*Harpoon*"
  "Name of the harpoon menu buffer.")

;; Core mark structure functions.
(defun davidc-harpoon--make-mark (&optional save-position)
  "Create a mark for the current buffer.
If SAVE-POSITION is nil, only saves buffer reference (no position).
If SAVE-POSITION is non-nil or not provided, saves exact cursor position."
  (let* ((buf (current-buffer))
         (is-dired (derived-mode-p 'dired-mode))
         (should-save-pos (not (eq save-position nil)))
         (type (cond (is-dired 'dired)
                    (t 'file)))
         (file (or (buffer-file-name)
                  (and is-dired default-directory)))
         (mark (list :buffer (buffer-name)
                    :file file
                    :type type
                    :position-saved should-save-pos
                    :timestamp (float-time))))

    ;; Always add position information unless explicitly disabled.
    (when should-save-pos
      (setq mark (plist-put mark :position (point)))
      (setq mark (plist-put mark :line (line-number-at-pos)))
      (setq mark (plist-put mark :column (current-column)))
      (setq mark (plist-put mark :preview (davidc-harpoon--get-preview-text))))

    mark))

(defun davidc-harpoon--get-preview-text ()
  "Get preview text for current position."
  (save-excursion
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      (string-trim (buffer-substring-no-properties start end)))))

(defun davidc-harpoon--update-timestamp (mark)
  "Update timestamp for MARK."
  (plist-put mark :timestamp (float-time)))

(defun davidc-harpoon--sort-by-lru (marks)
  "Sort MARKS by least recently used (most recent first)."
  (sort marks (lambda (a b)
                (> (plist-get a :timestamp)
                   (plist-get b :timestamp)))))

;; Mark management
(defun davidc-harpoon-mark-position (&optional slot)
  "Mark current position for quick navigation.
The exact cursor position is saved and will be restored when jumping.

With numeric prefix SLOT (C-1 through C-9): add to specific slot."
  (interactive "P")
  (davidc-harpoon--add-mark t slot))

(defun davidc-harpoon--add-mark (save-position &optional slot)
  "Internal function to add a mark.
If SAVE-POSITION is non-nil, save the exact position.
If SLOT is provided, add to that specific slot (1-9)."
  (when (and slot (or (< slot 1) (> slot davidc-harpoon-max-marks)))
    (user-error "Slot must be between 1 and %d" davidc-harpoon-max-marks))

  (let ((mark (davidc-harpoon--make-mark save-position)))

    (if slot
        ;; Add to specific slot.
        (davidc-harpoon--set-mark-at-slot mark (1- slot))
      ;; Add to next available slot or replace oldest.
      (if (< (length davidc-harpoon--marks) davidc-harpoon-max-marks)
          (setq davidc-harpoon--marks (append davidc-harpoon--marks (list mark)))
        ;; Replace oldest mark.
        (let ((sorted (davidc-harpoon--sort-by-lru davidc-harpoon--marks)))
          (setq davidc-harpoon--marks
                (append (list mark) (cdr sorted))))))

    (davidc-harpoon--save-marks)

    ;; Informative message.
    (message "Marked position: [%s] %s:%d"
             (or slot (length davidc-harpoon--marks))
             (plist-get mark :buffer)
             (or (plist-get mark :line) 1))))

(defun davidc-harpoon--set-mark-at-slot (mark slot)
  "Set MARK at specific SLOT (0-indexed)."
  (while (< (length davidc-harpoon--marks) (1+ slot))
    (setq davidc-harpoon--marks (append davidc-harpoon--marks (list nil))))
  (setf (nth slot davidc-harpoon--marks) mark))

(defun davidc-harpoon-clear-marks (&optional slot)
  "Clear harpoon mark at SLOT, or all marks if no slot given."
  (interactive "P")
  (if slot
      (when (and (>= slot 1) (<= slot (length davidc-harpoon--marks)))
        (setf (nth (1- slot) davidc-harpoon--marks) nil)
        (message "Cleared mark %d" slot))
    (setq davidc-harpoon--marks nil)
    (message "Cleared all marks"))
  (davidc-harpoon--save-marks)
  ;; Refresh menu if it's visible.
  (let ((menu-buf (get-buffer davidc-harpoon--menu-buffer-name)))
    (when menu-buf
      (with-current-buffer menu-buf
        (davidc-harpoon--refresh-menu)))))

;; Navigation functions.
(defun davidc-harpoon--jump-to-mark (mark)
  "Jump to the buffer or position specified by MARK."
  (when mark
    (davidc-harpoon--update-timestamp mark)
    (let ((file (plist-get mark :file))
          (buffer-name (plist-get mark :buffer))
          (pos (plist-get mark :position))
          (position-saved (plist-get mark :position-saved))
          (type (plist-get mark :type)))

      (cond
       ;; Dired directory.
       ((eq type 'dired)
        (dired file))

       ;; File mark.
       (file
        (if (file-exists-p file)
            (find-file file)
          ;; Try to find buffer even if file doesn't exist.
          (let ((buf (get-buffer buffer-name)))
            (if buf
                (switch-to-buffer buf)
              (message "File no longer exists: %s" file)))))

       ;; Buffer mark (no file).
       (t
        (let ((buf (get-buffer buffer-name)))
          (if buf
              (switch-to-buffer buf)
            (message "Buffer no longer exists: %s" buffer-name)))))

      ;; Go to specific position if available.
      (when pos
        (goto-char (min pos (point-max)))
        (recenter)))))

;; Generate jump functions for slots 1-9.
(dotimes (i 9)
  (let ((n (1+ i)))
    (eval `(defun ,(intern (format "davidc-harpoon-jump-to-%d" n)) ()
             ,(format "Jump to harpoon mark %d." n)
             (interactive)
             (let ((mark (nth ,(1- n) davidc-harpoon--marks)))
               (if mark
                   (davidc-harpoon--jump-to-mark mark)
                 (message "No mark at slot %d" ,n)))))))

;; Cycling functions.
(defun davidc-harpoon-next ()
  "Cycle to next mark in LRU order."
  (interactive)
  (when davidc-harpoon--marks
    (let* ((non-nil-marks (cl-remove-if #'null davidc-harpoon--marks))
           (sorted (davidc-harpoon--sort-by-lru non-nil-marks)))
      (when sorted
        ;; Move least recent to front.
        (let ((next (car (last sorted))))
          (davidc-harpoon--jump-to-mark next))))))

(defun davidc-harpoon-prev ()
  "Cycle to previous mark in LRU order."
  (interactive)
  (when davidc-harpoon--marks
    (let* ((non-nil-marks (cl-remove-if #'null davidc-harpoon--marks))
           (sorted (davidc-harpoon--sort-by-lru non-nil-marks)))
      (when (> (length sorted) 1)
        ;; Jump to second most recent.
        (davidc-harpoon--jump-to-mark (nth 1 sorted))))))

;; Menu interface
(defun davidc-harpoon-menu ()
  "Display the harpoon marks menu.
This opens a buffer showing all current marks. You can:
- Press 1-9 to jump to a specific mark
- Press RET to jump to mark at point
- Press 'd' to delete mark at point (with confirmation)
- Press 'x' to clear mark at point (no confirmation)
- Press 'c' to consolidate marks (remove gaps, optionally sort)
- Press 'D' to clear all marks
- Press 'g' to refresh the display
- Press 'q' to quit
- Use C-s to search within the menu"
  (interactive)
  (let ((buf (get-buffer-create davidc-harpoon--menu-buffer-name)))
    (with-current-buffer buf
      (davidc-harpoon-menu-mode)
      (davidc-harpoon--refresh-menu))
    (switch-to-buffer buf)))

(defun davidc-harpoon--refresh-menu ()
  "Refresh the harpoon menu display."
  (let ((inhibit-read-only t)
        (pos (point)))
    (erase-buffer)

    ;; Header.
    (insert (propertize "Harpoon Marks\n" 'face 'bold))
    (insert (propertize "═════════════\n\n" 'face 'bold))

    ;; Instructions.
    (insert "Keys: ")
    (insert (propertize "1-9" 'face 'font-lock-keyword-face))
    (insert " jump, ")
    (insert (propertize "RET" 'face 'font-lock-keyword-face))
    (insert " jump at point, ")
    (insert (propertize "d" 'face 'font-lock-keyword-face))
    (insert " delete (confirm), ")
    (insert (propertize "x" 'face 'font-lock-keyword-face))
    (insert " clear, ")
    (insert (propertize "c" 'face 'font-lock-keyword-face))
    (insert " consolidate\n")
    (insert "     ")
    (insert (propertize "D" 'face 'font-lock-keyword-face))
    (insert " clear all, ")
    (insert (propertize "g" 'face 'font-lock-keyword-face))
    (insert " refresh, ")
    (insert (propertize "q" 'face 'font-lock-keyword-face))
    (insert " quit, ")
    (insert (propertize "C-s" 'face 'font-lock-keyword-face))
    (insert " search\n\n")

    ;; Column headers.
    (insert (propertize "# " 'face 'bold))
    (insert (propertize "Buffer                          " 'face 'bold))
    (insert (propertize "Location  " 'face 'bold))
    (when davidc-harpoon-show-preview
      (insert (propertize "Preview" 'face 'bold)))
    (insert "\n")
    (insert (propertize "─ ────────────────────────────── ───────── " 'face 'bold))
    (when davidc-harpoon-show-preview
      (insert (propertize "─────────────────────────────────────────────" 'face 'bold)))
    (insert "\n")

    ;; Marks.
    (if (null davidc-harpoon--marks)
        (insert (propertize "No marks set. Use 'davidc-harpoon-mark-position' to add position marks.\n"
                           'face 'font-lock-comment-face))
      (dotimes (i davidc-harpoon-max-marks)
        (let ((mark (nth i davidc-harpoon--marks)))
          (davidc-harpoon--insert-mark-entry (1+ i) mark))))

    ;; Try to restore position.
    (goto-char (min pos (point-max)))))

(defun davidc-harpoon--insert-mark-entry (num mark)
  "Insert menu entry for mark NUM with data MARK."
  (let ((start (point)))
    (if mark
        (let* ((buffer-name (plist-get mark :buffer))
               (line (plist-get mark :line))
               (preview (plist-get mark :preview))
               (type (plist-get mark :type))
               (position-saved (plist-get mark :position-saved))
               (file (plist-get mark :file)))

          ;; Mark number.
          (insert (propertize (format "%d " num)
                             'face 'davidc-harpoon-mark-number))

          ;; Buffer/file name.
          (insert (propertize (format "%-32s "
                                     (truncate-string-to-width buffer-name 32 nil nil "..."))
                             'face 'davidc-harpoon-buffer-name))

          ;; Location indicator.
          (if (eq type 'dired)
              (insert (propertize "[DIR]     "
                                 'face 'davidc-harpoon-directory))
            (insert (propertize (format "L%-8d " line)
                               'face 'davidc-harpoon-line-number)))

          ;; Preview (only for non-directory marks).
          (when (and davidc-harpoon-show-preview (not (eq type 'dired)) preview)
            (insert (propertize
                    (truncate-string-to-width
                     preview
                     davidc-harpoon-preview-length nil nil "...")
                    'face 'davidc-harpoon-preview))))

      ;; Empty slot.
      (insert (propertize (format "%d " num)
                         'face 'davidc-harpoon-mark-number))
      (insert (propertize "(empty)" 'face 'shadow)))

    ;; Store the mark number as a text property for easy access.
    (put-text-property start (point) 'davidc-harpoon-mark-number num)
    (insert "\n")))

;; Menu mode.
(defvar davidc-harpoon-menu-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Number keys for jumping.
    (dotimes (i 9)
      (let ((n (1+ i)))
        (define-key map (kbd (number-to-string n))
          (intern (format "davidc-harpoon-menu-jump-%d" n)))))
    ;; Other commands.
    (define-key map (kbd "d") 'davidc-harpoon-menu-delete-at-point)
    (define-key map (kbd "x") 'davidc-harpoon-menu-clear-individual)
    (define-key map (kbd "c") 'davidc-harpoon-menu-consolidate)
    (define-key map (kbd "D") 'davidc-harpoon-menu-clear-all)
    (define-key map (kbd "g") 'davidc-harpoon--refresh-menu)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "RET") 'davidc-harpoon-menu-jump-at-point)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "?") 'describe-mode)
    map)
  "Keymap for harpoon menu mode.")

(define-derived-mode davidc-harpoon-menu-mode special-mode "Harpoon-Menu"
  "Major mode for the harpoon menu buffer.

This mode displays all harpoon marks and provides quick navigation.
You can search through marks using standard Emacs search (C-s).

\\{davidc-harpoon-menu-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local revert-buffer-function (lambda (_ignore-auto _noconfirm)
                                       (davidc-harpoon--refresh-menu))))

;; Generate menu jump functions.
(dotimes (i 9)
  (let ((n (1+ i)))
    (eval `(defun ,(intern (format "davidc-harpoon-menu-jump-%d" n)) ()
             ,(format "Jump to mark %d from menu." n)
             (interactive)
             (let ((mark (nth ,(1- n) davidc-harpoon--marks)))
               (if mark
                   (progn
                     (quit-window)
                     (davidc-harpoon--jump-to-mark mark))
                 (message "No mark at slot %d" ,n)))))))

(defun davidc-harpoon-menu-jump-at-point ()
  "Jump to mark at current line in menu."
  (interactive)
  (let ((mark-num (get-text-property (line-beginning-position) 'davidc-harpoon-mark-number)))
    (when mark-num
      (let ((mark (nth (1- mark-num) davidc-harpoon--marks)))
        (if mark
            (progn
              (quit-window)
              (davidc-harpoon--jump-to-mark mark))
          (message "No mark at slot %d" mark-num))))))

(defun davidc-harpoon-menu-delete-at-point ()
  "Delete mark at current line in menu."
  (interactive)
  (let ((mark-num (get-text-property (line-beginning-position) 'davidc-harpoon-mark-number)))
    (when mark-num
      (let ((mark (nth (1- mark-num) davidc-harpoon--marks)))
        (if mark
            (when (yes-or-no-p (format "Delete mark %d (%s)? "
                                       mark-num
                                       (plist-get mark :buffer)))
              (setf (nth (1- mark-num) davidc-harpoon--marks) nil)
              (davidc-harpoon--save-marks)
              (davidc-harpoon--refresh-menu)
              (message "Deleted mark %d" mark-num))
          (message "No mark at slot %d" mark-num))))))

(defun davidc-harpoon-menu-clear-individual ()
  "Clear individual mark at point without confirmation."
  (interactive)
  (let ((mark-num (get-text-property (line-beginning-position) 'davidc-harpoon-mark-number)))
    (when mark-num
      (let ((mark (nth (1- mark-num) davidc-harpoon--marks)))
        (if mark
            (progn
              (setf (nth (1- mark-num) davidc-harpoon--marks) nil)
              (davidc-harpoon--save-marks)
              (davidc-harpoon--refresh-menu)
              (message "Cleared mark %d" mark-num))
          (message "No mark at slot %d" mark-num))))))

(defun davidc-harpoon-menu-consolidate ()
  "Consolidate marks by removing empty gaps and optionally sorting."
  (interactive)
  (let* ((non-nil-marks (cl-remove-if #'null davidc-harpoon--marks))
         (sorted-marks (davidc-harpoon--sort-by-lru non-nil-marks)))
    (when non-nil-marks
      (if (yes-or-no-p "Sort marks by most recently used? (n for simple consolidation)")
          (setq davidc-harpoon--marks sorted-marks)
        (setq davidc-harpoon--marks non-nil-marks))
      ;; Pad with nils to maintain max-marks length.
      (while (< (length davidc-harpoon--marks) davidc-harpoon-max-marks)
        (setq davidc-harpoon--marks (append davidc-harpoon--marks (list nil))))
      (davidc-harpoon--save-marks)
      (davidc-harpoon--refresh-menu)
      (message "Consolidated marks: %d active marks" (length non-nil-marks)))))

(defun davidc-harpoon-menu-clear-all ()
  "Clear all marks from menu."
  (interactive)
  (when (yes-or-no-p "Clear all harpoon marks? ")
    (davidc-harpoon-clear-marks)
    (davidc-harpoon--refresh-menu)))

;; Persistence - save and restore marks from a file.
(defun davidc-harpoon--save-marks ()
  "Save marks to file if persistence is enabled."
  (when davidc-harpoon-persist-marks
    (with-temp-file davidc-harpoon-save-file
      (insert ";; Harpoon marks - auto-generated file\n")
      (insert ";; Do not edit manually\n\n")
      (pp davidc-harpoon--marks (current-buffer)))))

(defun davidc-harpoon--load-marks ()
  "Load marks from file if persistence is enabled."
  (when (and davidc-harpoon-persist-marks
             (file-exists-p davidc-harpoon-save-file))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents davidc-harpoon-save-file)
          (goto-char (point-min))
          ;; Skip comments.
          (while (and (not (eobp))
                      (looking-at "^;;"))
            (forward-line))
          (when (not (eobp))
            (setq davidc-harpoon--marks (read (current-buffer)))))
      (error
       (message "Error loading harpoon marks: %s" (error-message-string err))))))


;; Initialize on load.
(davidc-harpoon--load-marks)

(provide 'davidc-harpoon)
