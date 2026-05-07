;;; -*- lexical-binding: t -*-
;;
;; Format different types of buffers.
;;

;; Utility function to get platform-specific executable path.
;;
;; TODO: Merge this with the davidc-flymake--get-executable-path
;; function.
(defun davidc-format--get-executable-path (base-name)
  "Get the platform-specific executable path for BASE-NAME."
  (if (string-equal system-type "windows-nt")
      (concat base-name ".exe")
    base-name))

;; Define default settings if not already defined in 'custom-vars.el'.
(defvar davidc-python-format-ruff-path
  (davidc-format--get-executable-path "ruff")
  "Path to the ruff executable for Python formatting.
Set this variable before loading this package to use a custom path.")

(defvar davidc-format-html-prettier-path
  (davidc-format--get-executable-path "prettier")
  "Path to the prettier executable for HTML formatting.
Set this variable before loading this package to use a custom path.")


;; Tools
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


(defun davidc--format-python-region-with-file (program args)
  "Write the buffer to a temp .py file, run PROGRAM with ARGS and the file path.
Replaces buffer contents with the formatted result on success."
  (let ((temp-file (make-temp-file "python-format" nil ".py")))
    (unwind-protect
        (progn
          (write-region (point-min) (point-max) temp-file nil :quiet)
          (let ((exit-code (apply #'call-process program nil nil nil
                                  (append args (list temp-file)))))
            (if (zerop exit-code)
                (insert-file-contents temp-file nil nil nil t)
              (message "%s failed (exit code %d)" program exit-code))))
      (delete-file temp-file))))

(defun davidc-format-region-ruff (beg end)
  "Format a region of Python code using 'ruff format --range'.
Passes the full buffer file context; only the selected lines are modified."
  (interactive "r")
  (davidc--format-python-region-with-file
   davidc-python-format-ruff-path
   (list "format"
         (format "--range=%d-%d"
                 (line-number-at-pos beg)
                 (line-number-at-pos end)))))

(defun davidc-format-region-black (beg end)
  "Format a region of Python code using 'black --line-ranges'.
Passes the full buffer file context; only the selected lines are modified."
  (interactive "r")
  (davidc--format-python-region-with-file
   "black"
   (list (format "--line-ranges=%d-%d"
                 (line-number-at-pos beg)
                 (line-number-at-pos end)))))

(defun davidc-format-region-python ()
  "Format a region of Python code.
The formatter is controlled by `davidc-config-python-region-formatter'."
  (interactive)
  (cond
   ((eq davidc-config-python-region-formatter 'ruff)
    (call-interactively 'davidc-format-region-ruff))
   (t
    (call-interactively 'davidc-format-region-black))))

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

(defun davidc-format-buffer-html ()
  "Format an HTML buffer using prettier."
  (interactive)
  (let ((pos (point))
        (outbuf (generate-new-buffer " *prettier-format*")))
    (unwind-protect
        (let ((exit-code (call-process-region
                          (point-min) (point-max)
                          davidc-format-html-prettier-path
                          nil (list outbuf nil) nil
                          "--stdin-filepath" "dummy.html" "--parser" "html")))
          (if (zerop exit-code)
              (progn
                (erase-buffer)
                (insert-buffer-substring outbuf)
                (goto-char (min pos (point-max))))
            (message "prettier formatting failed (exit code %d)" exit-code)))
      (kill-buffer outbuf))))

(defun davidc-format-region ()
  "Format the selected region of text for supported major modes.
Supported major modes are C++ (c++-mode) and Python (python-mode).
For Rust (rust-mode) and HTML (html-mode), we format the entire buffer instead."
  (interactive)
  (cond
   ((string-equal major-mode "c++-mode") (call-interactively 'davidc-format-region-c++))
   ((string-equal major-mode "python-mode") (call-interactively 'davidc-format-region-python))
   ((string-equal major-mode "rust-mode")
    (message "Region formatting not supported for Rust - formatting entire buffer")
    (call-interactively 'davidc-format-buffer-rust))
   ((or (string-equal major-mode "html-mode")
        (string-equal major-mode "mhtml-mode"))
    (message "Region formatting not supported for HTML - formatting entire buffer")
    (call-interactively 'davidc-format-buffer-html))
   ))

(defun davidc-format-buffer ()
  "Format the selected buffer of text for supported major modes.
Supported major modes are C++ (c++-mode), Python (python-mode),
Rust (rust-mode) and HTML (html-mode)."
  (interactive)
  (cond
   ((string-equal major-mode "c++-mode") (call-interactively 'davidc-format-buffer-c++))
   ((string-equal major-mode "python-mode") (call-interactively 'davidc-format-buffer-python))
   ((string-equal major-mode "rust-mode") (call-interactively 'davidc-format-buffer-rust))
   ((or (string-equal major-mode "html-mode")
        (string-equal major-mode "mhtml-mode"))
    (call-interactively 'davidc-format-buffer-html))
   )
  )

(defun davidc-format ()
  "Format the buffer or region for supported major modes.
Supported major modes are C++ (c++-mode), Python (python-mode),
Rust (rust-mode) and HTML (html-mode)."
  (interactive)
  ;; http://xahlee.info/emacs/emacs/emacs_region.html
  (if (use-region-p)
      (call-interactively 'davidc-format-region)
    (call-interactively 'davidc-format-buffer)))


(provide 'davidc-format)
