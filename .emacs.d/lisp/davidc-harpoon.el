;;; -*- lexical-binding: t -*-
;;
;; Harpoon-inspired navigation using native Emacs features.
;;
;; This package provides a streamlined way to mark and quickly jump
;; between important locations using Emacs native registers and
;; bookmarks.
;;
;; FEATURES:
;; - Mark positions in registers 1-9 using native Emacs registers
;; - Quick jump with single keystroke (1-9)
;; - Bookmark support for named, persistent marks
;; - Automatic integration with Emacs bookmark system
;;
;; BASIC USAGE:
;; Mark position in register: M-x davidc-harpoon-mark-register-N (N=1-9)
;; Jump to register N: M-x davidc-harpoon-jump-to-N (N=1-9)
;; Set bookmark: M-x davidc-harpoon-bookmark-set
;; Jump to bookmark: M-x davidc-harpoon-bookmark-jump
;; List bookmarks: M-x bookmark-bmenu-list (or C-x r l)
;;
;; SUGGESTED KEY BINDINGS:
;; (global-set-key (kbd "C-c h m") 'davidc-harpoon-mark-position)
;; (global-set-key (kbd "C-c h b") 'davidc-harpoon-bookmark-set)
;; (global-set-key (kbd "C-c h j") 'davidc-harpoon-bookmark-jump)
;; (global-set-key (kbd "C-c h l") 'bookmark-bmenu-list)
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

(require 'bookmark)

(defgroup davidc-harpoon nil
  "Simplified navigation system using native Emacs registers and bookmarks."
  :group 'convenience
  :prefix "davidc-harpoon-")

(defcustom davidc-harpoon-default-register-action 'ask
  "Default action when marking a position.
'register - always use numbered register
'bookmark - always create bookmark
'ask - prompt user to choose"
  :type '(choice (const :tag "Use register" register)
                 (const :tag "Use bookmark" bookmark)
                 (const :tag "Ask user" ask))
  :group 'davidc-harpoon)

;; Register-based position marking (1-9)
(defun davidc-harpoon-mark-register (n)
  "Mark current position in register N (1-9)."
  (when (and (>= n 1) (<= n 9))
    (let ((register (+ ?0 n)))  ; Convert 1-9 to ?1-?9
      (point-to-register register)
      (message "Marked position in register %d: %s:%d"
               n (buffer-name) (line-number-at-pos)))))

(defun davidc-harpoon-jump-to-register (n)
  "Jump to position in register N (1-9)."
  (when (and (>= n 1) (<= n 9))
    (let ((register (+ ?0 n)))  ; Convert 1-9 to ?1-?9
      (condition-case nil
          (progn
            (jump-to-register register)
            (message "Jumped to register %d" n))
        (error (message "No mark set in register %d" n))))))

;; Generate register functions for 1-9
(dotimes (i 9)
  (let ((n (1+ i)))
    (eval `(defun ,(intern (format "davidc-harpoon-mark-register-%d" n)) ()
             ,(format "Mark current position in register %d." n)
             (interactive)
             (davidc-harpoon-mark-register ,n)))

    (eval `(defun ,(intern (format "davidc-harpoon-jump-to-%d" n)) ()
             ,(format "Jump to position in register %d." n)
             (interactive)
             (davidc-harpoon-jump-to-register ,n)))))

;; Bookmark helpers
(defun davidc-harpoon-bookmark-set (&optional name)
  "Set a bookmark with automatic naming if NAME not provided."
  (interactive)
  (let ((bookmark-name (or name
                          (format "%s:%d"
                                  (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
                                  (line-number-at-pos)))))
    (bookmark-set bookmark-name)
    (message "Bookmark set: %s" bookmark-name)))

(defun davidc-harpoon-bookmark-jump ()
  "Quick bookmark selection and jump."
  (interactive)
  (if (bookmark-maybe-load-default-file)
      (let ((bookmark (completing-read "Jump to bookmark: "
                                       (bookmark-all-names)
                                       nil t)))
        (when (and bookmark (not (string-empty-p bookmark)))
          (bookmark-jump bookmark)))
    (message "No bookmarks available")))

;; Main marking function that respects user preference
(defun davidc-harpoon-mark-position (&optional force-type)
  "Mark current position using registers or bookmarks.
With prefix argument, prompt for specific register number.
Behavior depends on `davidc-harpoon-default-register-action'."
  (interactive "P")
  (cond
   ;; Prefix argument: prompt for register number
   (force-type
    (let ((n (read-number "Register number (1-9): " 1)))
      (if (and (>= n 1) (<= n 9))
          (davidc-harpoon-mark-register n)
        (user-error "Register number must be between 1 and 9"))))

   ;; Use default action
   ((eq davidc-harpoon-default-register-action 'register)
    (davidc-harpoon-mark-next-available-register))

   ((eq davidc-harpoon-default-register-action 'bookmark)
    (davidc-harpoon-bookmark-set))

   ;; Ask user
   (t
    (let ((choice (read-char-choice
                   "Mark as: (r)egister, (b)ookmark, (1-9) specific register? "
                   '(?r ?b ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))))
      (cond
       ((eq choice ?r) (davidc-harpoon-mark-next-available-register))
       ((eq choice ?b) (davidc-harpoon-bookmark-set))
       ((and (>= choice ?1) (<= choice ?9))
        (davidc-harpoon-mark-register (- choice ?0))))))))

(defun davidc-harpoon-mark-next-available-register ()
  "Mark position in next available register (1-9)."
  (let ((found nil)
        (n 1))
    (while (and (not found) (<= n 9))
      (let ((register (+ ?0 n)))
        (if (not (get-register register))
            (progn
              (davidc-harpoon-mark-register n)
              (setq found t))
          (setq n (1+ n)))))
    (unless found
      ;; All registers occupied, use register 1
      (davidc-harpoon-mark-register 1)
      (message "All registers occupied, overwrote register 1"))))

;; Quick status functions
(defun davidc-harpoon-show-registers ()
  "Show status of all harpoon registers (1-9)."
  (interactive)
  (let ((status-lines '("Harpoon Registers:")))
    (dotimes (i 9)
      (let* ((n (1+ i))
             (register (+ ?0 n))
             (marker (get-register register)))
        (if marker
            (push (format "  %d: %s" n
                         (if (markerp marker)
                             (format "%s:%d"
                                    (buffer-name (marker-buffer marker))
                                    (with-current-buffer (marker-buffer marker)
                                      (line-number-at-pos (marker-position marker))))
                           "Unknown"))
                  status-lines)
          (push (format "  %d: (empty)" n) status-lines))))
    (message "%s" (string-join (reverse status-lines) "\n"))))

(defun davidc-harpoon-clear-registers ()
  "Clear all harpoon registers (1-9)."
  (interactive)
  (when (yes-or-no-p "Clear all harpoon registers (1-9)? ")
    (dotimes (i 9)
      (let ((register (+ ?1 i)))  ; ?1 to ?9
        (set-register register nil)))
    (message "Cleared all harpoon registers")))

(provide 'davidc-harpoon)
