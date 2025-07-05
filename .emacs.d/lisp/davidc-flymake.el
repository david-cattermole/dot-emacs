;;; -*- lexical-binding: t -*-
;;
;; Flymake integration, for Python, Rust and C++.
;;
;; Python uses 'ruff'.
;; Rust uses 'clippy'.
;; C++ uses 'clang-tidy'.
;;

;; Define default settings if not already defined in 'custom-vars.el'.
(defvar davidc-python-flymake-ruff-path
  (cond
   ((string-equal system-type "windows-nt") ; Windows
    "ruff.exe")
   ((string-equal system-type "darwin") ; macOS
    "ruff")
   (t ; Linux or other
    "ruff")))

;; Setup flymake configuration for Python with ruff.
(defun davidc-python-flymake-ruff-setup ()
  "Configure flymake for Python using ruff."
  (when (and (boundp 'python-flymake-command)
             (boundp 'python-flymake-command-output-pattern)
             (boundp 'python-flymake-msg-alist))
    ;; Use the settings from custom-vars.el if they exist
    ;; Otherwise, use default values
    (unless (symbol-value 'python-flymake-command)
      (setq-local python-flymake-command
                  (list davidc-python-flymake-ruff-path "check"
                        "--stdin-filename=<stdin>"
                        "-")))

    (unless (symbol-value 'python-flymake-command-output-pattern)
      (setq-local python-flymake-command-output-pattern
                  '("^\\(?:<?stdin>?\\):\\(?1:[0-9]+\\):\\(?:\\(?2:[0-9]+\\):\\)? \\(?3:.*\\)$" 1 2 nil 3)))

    (unless (symbol-value 'python-flymake-msg-alist)
      (setq-local python-flymake-msg-alist
                  '(("\\(^redefinition\\|.*unused.*\\|used$\\)" . :warning)
                    ("^E999" . :error)
                    ("^[EW][0-9]+" . :note))))))

;; Define default settings for clang-tidy path.
(defvar davidc-flymake-clang-tidy-path
  (cond
   ((string-equal system-type "windows-nt") ; Windows
    "clang-tidy.exe")
   ((string-equal system-type "darwin") ; macOS
    "clang-tidy")
   (t ; Linux or other
    "clang-tidy"))
  "Path to the clang-tidy executable.")

;; Local variable to keep track of the currently running flymake
;; clang-tidy process.
(defvar-local davidc--flymake-clang-tidy-proc nil
  "Current clang-tidy flymake process.")

(defun davidc-flymake-clang-tidy-on-save ()
  "Run Flymake only when buffer is saved."
  (when flymake-mode
    (flymake-start)))

;; Add Clang-tidy as a backend for Flymake.
;;
;; This uses this annotated example for ruby as a starting point:
;; https://www.gnu.org/software/emacs/manual/html_node/flymake/An-annotated-example-backend.html
(defun davidc-flymake-clang-tidy (report-fn &rest _args)
  "Flymake backend for clang-tidy.
REPORT-FN is the callback function for reporting diagnostics."
  ;; Check if clang-tidy exists.
  (unless (executable-find davidc-flymake-clang-tidy-path)
    (error "Flymake mode clang-tidy; Cannot find clang-tidy executable at \"%s\"." davidc-flymake-clang-tidy-path))
  ;; (message "[DEBUG] Flymake mode clang-tidy; using clang-tidy executable at \"%s\"." davidc-flymake-clang-tidy-path)

  ;; Kill any existing process.
  (when (process-live-p davidc--flymake-clang-tidy-proc)
    (kill-process davidc--flymake-clang-tidy-proc))

  ;; Save the current buffer.
  (let* ((source (current-buffer))
         (source-file (buffer-file-name)))

    (when source-file
      ;; When this runs from 'after-save-hook', the buffer is already
      ;; saved. If we need to ensure the file is saved, we should save
      ;; it ourselves rather than erroring out.
      (when (buffer-modified-p)
        (save-buffer))

      ;; (message "[DEBUG] Flymake mode clang-tidy; Source File: \"%s\"." source-file)
      (save-restriction
        (widen)

        ;; Reset the process variable and create a new process with
        ;; improved options.
        (setq davidc--flymake-clang-tidy-proc
              (make-process
               :name "flymake-clang-tidy"
               :noquery t
               :connection-type 'pipe
               :buffer (generate-new-buffer " *flymake-clang-tidy*")
               ;; ':file-handler' will ensure the current working
               ;; directory is set to the current buffer's value of
               ;; 'default-directory', or '~' as a fallback.
               :file-handler t
               :command (list davidc-flymake-clang-tidy-path
                              "-quiet"
                              (file-name-nondirectory source-file)
                              ;; NOTE: Passing this "--" seems to call
                              ;; the compiler directly. On Windows
                              ;; that is not possible, so we don't get
                              ;; much value from enabling this.
                              ;;
                              ;; "--"
                              )
               :sentinel
               (lambda (proc _event)
                 ;; Check that the process has indeed exited.
                 (when (memq (process-status proc) '(exit signal))
                   (unwind-protect
                       ;; Only proceed if proc is the current process.
                       (if (with-current-buffer source (eq proc davidc--flymake-clang-tidy-proc))
                           (with-current-buffer (process-buffer proc)
                             ;; ;; Debug output - print the entire buffer content
                             ;; (let ((output-content (buffer-string)))
                             ;;   (message "[DEBUG] clang-tidy output: \n%s" output-content))

                             (goto-char (point-min))
                             (let ((diags '()))
                               ;; Update regex to capture the file path as well
                               (while (search-forward-regexp
                                       "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(warning\\|error\\|note\\): \\(.*\\)$"
                                       nil t)
                                 (let* ((full-match (match-string 0))
                                        (file-path (match-string 1))
                                        (line (string-to-number (match-string 2)))
                                        (col (string-to-number (match-string 3)))
                                        (type (match-string 4))
                                        (msg (match-string 5))
                                        (level (cond
                                                ((string= type "error") :error)
                                                ((string= type "warning") :warning)
                                                (t :note)))

                                        ;; Get region for diagnostics.
                                        (beg-end (flymake-diag-region
                                                 source
                                                 line
                                                 col)))

                                   ;; ;; Debug output for each match
                                   ;; (message "[DEBUG] Match: %s" full-match)
                                   ;; (message "[DEBUG] File: %s" file-path)
                                   ;; (message "[DEBUG] Current file: %s" source-file)
                                   ;; (message "[DEBUG] Line: %d, Col: %d, Type: %s" line col type)
                                   ;; (message "[DEBUG] Message: %s" msg)
                                   ;; (message "[DEBUG] Level: %s" level)

                                   ;; Check if this diagnostic is for the current file
                                   ;; Try to match by comparing normalized paths
                                   (let* ((normalized-source (expand-file-name source-file))
                                          (normalized-file (expand-file-name (directory-file-name file-path)))
                                          (is-current-file (or
                                                            (string= normalized-file normalized-source)
                                                            (string-suffix-p (file-name-nondirectory source-file) file-path))))

                                     ;; (message "[DEBUG] Is current file: %s" is-current-file)

                                     ;; Only add diagnostics for the current file, and ignore notes.
                                     (when (and is-current-file
                                                beg-end
                                                (not (eq level :note))) ; Skip note-level diagnostics
                                       ;; (message "[DEBUG] Adding diagnostic for current file")
                                       ;; (message "[DEBUG] Region: %s-%s" (car beg-end) (cdr beg-end))
                                       (push (flymake-make-diagnostic source
                                                                    (car beg-end)
                                                                    (cdr beg-end)
                                                                    level
                                                                    msg)
                                             diags)))))

                               ;; Report the diagnostics.
                               (message "Flymake mode clang-tidy; Found %d diagnostics for \"%s\"."
                                       (length diags)
                                       (file-name-nondirectory source-file))
                               (funcall report-fn diags)))

                         ;; If obsolete, log warning.
                         (flymake-log :warning "Canceling obsolete check %s" proc))

                     ;; Clean up resources.
                     (when (process-buffer proc)
                       (kill-buffer (process-buffer proc))))))))))))

(defun davidc-flymake-clang-tidy-setup ()
  "Set up flymake for clang-tidy."
  (interactive)
  (message "Setting up clang-tidy for flymake.")
  ;; Add our clang-tidy backend to the list of diagnostic functions.
  (add-hook 'flymake-diagnostic-functions #'davidc-flymake-clang-tidy nil t)
  ;; Enable flymake.
  (flymake-mode 1))


;; Define default settings for Rust's Cargo executable path.
(defvar davidc-flymake-rust-cargo-path
  (cond
   ((string-equal system-type "windows-nt") ; Windows
    "cargo.exe")
   ((string-equal system-type "darwin") ; macOS
    "cargo")
   (t ; Linux or other
    "cargo"))
  "Path to the Rust cargo executable.")

(defvar davidc-flymake-rust-cargo-clippy-args
  '()
  "Arguments to pass to cargo clippy.
This should be a list of strings, each element being a separate argument.
By default, no extra arguments are passed, using Clippy's default settings.

Some common options include:
* (\"-W\" \"clippy::all\") - Enable all clippy warnings
* (\"-W\" \"clippy::pedantic\") - Enable pedantic clippy warnings
* (\"-A\" \"clippy::some_lint\") - Disable a specific lint

See `cargo clippy --help` for more options.")

;; Local variable to keep track of the currently running flymake
;; rust-cargo-clippy process.
(defvar-local davidc--flymake-rust-cargo-clippy-proc nil
  "Current cargo clippy flymake process.")

(defun davidc-flymake-rust-cargo-clippy-on-save ()
  "Run Flymake only when buffer is saved."
  (when flymake-mode
    (flymake-start)))

(defun davidc-flymake-rust-cargo-clippy (report-fn &rest _args)
  "Flymake backend for Rust's clippy.
REPORT-FN is the callback function for reporting diagnostics."
  ;; Check if rust-cargo-clippy exists.
  (unless (executable-find davidc-flymake-rust-cargo-path)
    (error "Flymake mode rust-cargo-clippy; Cannot find cargo executable at \"%s\"." davidc-flymake-rust-cargo-path))
  ;; (message "[DEBUG] Flymake mode rust-cargo-clippy; using cargo executable at \"%s\"." davidc-flymake-rust-cargo-path)

  ;; Kill any existing process.
  (when (process-live-p davidc--flymake-rust-cargo-clippy-proc)
    (kill-process davidc--flymake-rust-cargo-clippy-proc))

  ;; Save the current buffer.
  (let* ((source (current-buffer))
         (source-file (buffer-file-name)))

    (when source-file
      ;; When this runs from 'after-save-hook', the buffer is already
      ;; saved. If we need to ensure the file is saved, we should save
      ;; it ourselves rather than erroring out.
      (when (buffer-modified-p)
        (save-buffer))

      ;; (message "[DEBUG] Flymake mode rust-cargo-clippy; Source File: \"%s\"." source-file)
      (save-restriction
        (widen)

        ;; Get the path components for the current file for later comparison
        (let* ((source-dir (file-name-directory source-file))
               (source-basename (file-name-nondirectory source-file))
               (proj-dir (locate-dominating-file source-file "Cargo.toml")))

          ;; If we can't find a Cargo.toml, warn the user
          (unless proj-dir
            (message "Warning: No Cargo.toml found in parent directories of %s" source-file))

          ;; Create the process
          (setq davidc--flymake-rust-cargo-clippy-proc
                (make-process
                 :name "flymake-rust-cargo-clippy"
                 :noquery t
                 :connection-type 'pipe
                 :buffer (generate-new-buffer " *flymake-rust-cargo-clippy*")
                 ;; ':file-handler' will ensure the current working
                 ;; directory is set to the current buffer's value of
                 ;; 'default-directory', or '~' as a fallback.
                 :file-handler t
                 :command (append (list davidc-flymake-rust-cargo-path
                                        "clippy"
                                        "--message-format=short"
                                        "--")
                                  davidc-flymake-rust-cargo-clippy-args)
                 :sentinel
                 (lambda (proc _event)
                   ;; Check that the process has indeed exited.
                   (when (memq (process-status proc) '(exit signal))
                     (unwind-protect
                         ;; Only proceed if proc is the current process.
                         (if (with-current-buffer source (eq proc davidc--flymake-rust-cargo-clippy-proc))
                             (with-current-buffer (process-buffer proc)
                               ;; ;; Debug output - print the entire buffer content
                               ;; (let ((output-content (buffer-string)))
                               ;;   (message "[DEBUG] rust-cargo-clippy full output:\n%s" output-content))

                               (goto-char (point-min))
                               (let ((diags '()))

                                 ;; Parse the Rust error/warning lines directly
                                 ;; Format: file:line:col: warning/error: message
                                 (while (re-search-forward "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(warning\\|error\\(?:\\[E[0-9]+\\]\\)?\\): \\(.*\\)$" nil t)
                                   (let* ((file-path (match-string 1))
                                          (line (string-to-number (match-string 2)))
                                          (col (string-to-number (match-string 3)))
                                          (type-str (match-string 4))
                                          (message-text (match-string 5))
                                          (level (if (string-match-p "^error" type-str) :error :warning))
                                          (file-basename (file-name-nondirectory file-path))
                                          (is-current-file (string= file-basename source-basename)))

                                     ;; (message "[DEBUG] Found: %s at %s:%d:%d - %s"
                                     ;;          type-str file-path line col message-text)
                                     ;; (message "[DEBUG] Current file: %s, Parsed file: %s, Match: %s"
                                     ;;          source-basename file-basename is-current-file)

                                     ;; Check if this is for the current file by comparing basenames
                                     (when is-current-file
                                       (let ((beg-end (flymake-diag-region source line col)))
                                         (when beg-end
                                           ;; (message "[DEBUG] Adding diagnostic: %s at %d:%d"
                                           ;;          message-text line col)
                                           (push (flymake-make-diagnostic source
                                                                          (car beg-end)
                                                                          (cdr beg-end)
                                                                          level
                                                                          message-text)
                                                 diags))))))

                                 ;; Report the diagnostics.
                                 (message "Flymake mode rust-cargo-clippy; Found %d diagnostics for \"%s\"."
                                          (length diags)
                                          (file-name-nondirectory source-file))
                                 (funcall report-fn diags)))

                           ;; If obsolete, log warning.
                           (flymake-log :warning "Canceling obsolete check %s" proc))

                       ;; Clean up resources.
                       (when (process-buffer proc)
                         (kill-buffer (process-buffer proc)))))))))))))

(defun davidc-flymake-rust-cargo-clippy-setup ()
  "Set up flymake for rust clippy."
  (interactive)
  (message "Setting up Rust clippy for flymake.")
  ;; Add our rust clippy backend to the list of diagnostic functions.
  (add-hook 'flymake-diagnostic-functions #'davidc-flymake-rust-cargo-clippy nil t)
  ;; Enable flymake.
  (flymake-mode 1))


(defun davidc-flymake-get-sorted-diagnostics (reverse)
  "Get all Flymake diagnostics sorted by position.
If REVERSE is non-nil, sort in reverse order."
  (let ((diagnostics (flymake-diagnostics)))
    (sort diagnostics
          (lambda (a b)
            (if reverse
                (> (flymake-diagnostic-beg a) (flymake-diagnostic-beg b))
              (< (flymake-diagnostic-beg a) (flymake-diagnostic-beg b)))))))

(defun davidc-flymake-diagnostic-matches-p (diag level current-line current-point direction wrapped)
  "Check if diagnostic DIAG matches the search criteria.
LEVEL is the diagnostic level to match (:error, :warning, or :note).
CURRENT-LINE and CURRENT-POINT define the current position.
DIRECTION can be 'next or 'prev to determine search direction.
WRAPPED if non-nil means we're searching after wrapping around the buffer."
  (and
   ;; Check position based on direction and wrapped state.
   (if wrapped
       ;; When wrapped, we're searching the entire buffer.
       t
     ;; When not wrapped, respect direction.
     (if (eq direction 'next)
         ;; For next: find diagnostics after current point.
         (or (> (line-number-at-pos (flymake-diagnostic-beg diag)) current-line)
             (and (= (line-number-at-pos (flymake-diagnostic-beg diag)) current-line)
                  (> (flymake-diagnostic-beg diag) current-point)))
       ;; For prev: find diagnostics before current point.
       (or (< (line-number-at-pos (flymake-diagnostic-beg diag)) current-line)
           (and (= (line-number-at-pos (flymake-diagnostic-beg diag)) current-line)
                (< (flymake-diagnostic-beg diag) current-point)))))
   ;; Check for exact type match.
   (eq (flymake-diagnostic-type diag) level)))

(defun davidc-flymake-goto-diagnostic (direction &optional level)
  "Go to the next/previous Flymake diagnostic with specified LEVEL.
Will wrap around the buffer if no matching diagnostic is found.

DIRECTION can be 'next or 'prev.
LEVEL can be :error, :warning, or :note. If not specified, use :error."
  (interactive)
  ;; Check if flymake is running in the current buffer.
  (if (not flymake-mode)
      (message "Flymake not running in current buffer")

    (let* ((level-value (or level :error))
           (current-line (line-number-at-pos))
           (current-point (point))
           (diagnostics (davidc-flymake-get-sorted-diagnostics (eq direction 'prev)))
           (found nil)
           (wrapped nil)
           (level-name (cond
                        ((eq level-value :error) "error")
                        ((eq level-value :warning) "warning")
                        ((eq level-value :note) "note"))))

      ;; If there are no diagnostics at all, inform the user and exit early
      (if (null diagnostics)
          (message "No diagnostics found in buffer")

    ;; First attempt: search in specified direction without wrapping.
    (dolist (diag diagnostics)
      (when (and (not found)
                 (davidc-flymake-diagnostic-matches-p
                  diag level-value current-line current-point direction wrapped))
        (goto-char (flymake-diagnostic-beg diag))
        (setq found t)))

    ;; Second attempt: if not found, search again with wrapping.
    (unless found
      (setq wrapped t)
      ;; For wrapping, consider all diagnostics of the specified type,
      ;; regardless of their position relative to point.
      (dolist (diag diagnostics)
        (when (and (not found)
                   (davidc-flymake-diagnostic-matches-p
                    diag level-value current-line current-point direction wrapped))
          (goto-char (flymake-diagnostic-beg diag))
          (setq found t)))

      ;; If we found one on the wrap, show a more descriptive message.
      (when found
        (message "Reached %s of buffer, wrapped to %s and found %s %s"
                 (if (eq direction 'next) "end" "beginning")
                 (if (eq direction 'next) "beginning" "end")
                 (if (eq level-value :error) "an" "a")
                 level-name)))

    ;; Show message if nothing found at all.
    (unless found
      (message "No %s diagnostics found in buffer" level-name))))))

;; Define convenience functions.
(defun flymake-next-error ()
  "Go to the next Flymake error.
The search will wrap around the buffer if needed."
  (interactive)
  (davidc-flymake-goto-diagnostic 'next :error))

(defun flymake-prev-error ()
  "Go to the previous Flymake error.
The search will wrap around the buffer if needed."
  (interactive)
  (davidc-flymake-goto-diagnostic 'prev :error))

(defun flymake-next-warning ()
  "Go to the next Flymake warning.
The search will wrap around the buffer if needed."
  (interactive)
  (davidc-flymake-goto-diagnostic 'next :warning))

(defun flymake-prev-warning ()
  "Go to the previous Flymake warning.
The search will wrap around the buffer if needed."
  (interactive)
  (davidc-flymake-goto-diagnostic 'prev :warning))

(defun flymake-next-note ()
  "Go to the next Flymake note.
The search will wrap around the buffer if needed."
  (interactive)
  (davidc-flymake-goto-diagnostic 'next :note))

(defun flymake-prev-note ()
  "Go to the previous Flymake note.
The search will wrap around the buffer if needed."
  (interactive)
  (davidc-flymake-goto-diagnostic 'prev :note))

(provide 'davidc-flymake)
