;;; -*- lexical-binding: t -*-
;;
;; Simple functions to restart Emacs, with open buffers re-opened at
;; start-up.
;;

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

(provide 'davidc-restart)
