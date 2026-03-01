;;; -*- lexical-binding: t -*-
;;
;; Quick access to documentation, diagnostics, and inline help.
;;
;; FEATURES:
;; * Toggle eldoc documentation buffer.
;; * Show brief eldoc summary in the minibuffer.
;; * Toggle flymake diagnostics buffer.
;; * Look up eldoc for symbol at point.
;; * Navigate flymake errors, warnings, and notes.
;; * Eglot hover documentation at point.
;;
;; BASIC SETUP:
;;   (require 'davidc-quick-docs)
;;

(require 'eldoc)
(require 'flymake)

(defgroup davidc-quick-docs nil
  "Quick access to documentation and diagnostics."
  :group 'convenience)

(defcustom davidc-quick-docs-minibuffer-lines 3
  "Number of lines to show in the minibuffer for the eldoc summary."
  :type 'integer
  :group 'davidc-quick-docs)


;;; Eldoc helpers.

(defun davidc-quick-docs--eldoc-buffer ()
  "Return the live eldoc documentation buffer for the current buffer, or nil.

Uses the internal `eldoc--doc-buffer' variable which holds the
actual buffer object, avoiding assumptions about the buffer name."
  (and (boundp 'eldoc--doc-buffer)
       (buffer-live-p eldoc--doc-buffer)
       eldoc--doc-buffer))

(defun davidc-quick-docs--eldoc-show-buffer ()
  "Show the eldoc documentation buffer via `eldoc-doc-buffer'."
  (condition-case nil
      (eldoc-doc-buffer t)
    (error (message "No eldoc documentation available."))))


;;; Eldoc commands.

(defun davidc-quick-docs-eldoc-show-minibuffer ()
  "Show the first few lines of eldoc documentation in the minibuffer.

The number of lines shown is controlled by
`davidc-quick-docs-minibuffer-lines'.  If no eldoc buffer exists
yet, triggers the backends and relies on the echo-area display
configured in `eldoc-display-functions'."
  (interactive)
  (let ((buf (davidc-quick-docs--eldoc-buffer)))
    (if buf
        (let* ((content (with-current-buffer buf
                          (buffer-substring-no-properties (point-min) (point-max))))
               (lines (seq-take (split-string content "\n" t)
                                davidc-quick-docs-minibuffer-lines))
               (text (string-join lines "\n")))
          (if (string-empty-p (string-trim text))
              (message "No eldoc documentation available.")
            (message "%s" text)))
      ;; No buffer yet - trigger backends; echo-area display will handle output.
      (eldoc-print-current-symbol-info))))

(defun davidc-quick-docs-eldoc-toggle-buffer ()
  "Toggle the eldoc documentation buffer.

If the eldoc buffer is visible in any window, delete that window.
Otherwise, trigger the eldoc backends and display the buffer.

Uses `eldoc--doc-buffer' to find the buffer regardless of its name."
  (interactive)
  (let* ((buf (davidc-quick-docs--eldoc-buffer))
         (win (and buf (get-buffer-window buf))))
    (if win
        ;; Buffer is visible - hide it.
        (delete-window win)
      ;; Trigger eldoc backends to populate the buffer, then show it
      ;; once they have had a chance to run.
      (eldoc-print-current-symbol-info)
      (run-with-timer 0.1 nil #'davidc-quick-docs--eldoc-show-buffer))))


;;; Flymake.

(defun davidc-quick-docs-flymake-toggle ()
  "Toggle the flymake diagnostics buffer for the current buffer.

If the diagnostics buffer is visible, close it. Otherwise, open it."
  (interactive)
  (if (not (bound-and-true-p flymake-mode))
      (message "Flymake is not active in this buffer.")
    (let* ((buf-name (flymake--diagnostics-buffer-name))
           (buf (get-buffer buf-name))
           (win (and buf (get-buffer-window buf t))))
      (cond
       (win
        ;; Buffer is visible - close it.
        (quit-window nil win))
       (t
        ;; Buffer not visible - open it.
        (flymake-show-buffer-diagnostics))))))

(defun davidc-quick-docs-flymake-next-error ()
  "Jump to the next flymake error in the current buffer."
  (interactive)
  (flymake-goto-next-error nil '(:error) t))

(defun davidc-quick-docs-flymake-prev-error ()
  "Jump to the previous flymake error in the current buffer."
  (interactive)
  (flymake-goto-prev-error nil '(:error) t))

(defun davidc-quick-docs-flymake-next-warning ()
  "Jump to the next flymake warning in the current buffer."
  (interactive)
  (flymake-goto-next-error nil '(:warning) t))

(defun davidc-quick-docs-flymake-prev-warning ()
  "Jump to the previous flymake warning in the current buffer."
  (interactive)
  (flymake-goto-prev-error nil '(:warning) t))

(defun davidc-quick-docs-flymake-next-note ()
  "Jump to the next flymake note in the current buffer."
  (interactive)
  (flymake-goto-next-error nil '(:note) t))

(defun davidc-quick-docs-flymake-prev-note ()
  "Jump to the previous flymake note in the current buffer."
  (interactive)
  (flymake-goto-prev-error nil '(:note) t))


(provide 'davidc-quick-docs)
