;;; -*- lexical-binding: t -*-
;;
;; Enhanced Flymake integration for Python, Rust, C++, JSON with
;; comments, YAML, and XML.
;;
;; This package provides streamlined Flymake backends for multiple
;; programming languages, offering real-time syntax checking and
;; linting capabilities.
;;
;; SUPPORTED LANGUAGES:
;; * Python - Uses "ruff" for fast Python linting.
;; * Rust   - Uses "cargo clippy" for comprehensive Rust analysis.
;; * C++    - Uses "clang-tidy" for static analysis.
;; * JSON   - Uses custom Python script for JSON with comments.
;; * YAML   - Uses custom Python script for YAML.
;; * XML    - Uses custom Python script for XML validation.
;;
;; PREREQUISITES:
;; Make sure the following tools are installed and available in your PATH:
;; * For Python: ruff (install via: pip install ruff)
;; * For Rust: cargo with clippy component (rustup component add clippy)
;; * For C++: clang-tidy (part of LLVM/Clang toolchain)
;; * For JSON: Python 3 and the jsonc_lint.py script
;; * For YAML: Python 3 and the yaml_lint.py script
;; * For XML: Python 3 and the xml_lint.py script
;;
;; BASIC SETUP:
;; 1. Load this file in your Emacs configuration
;; 2. Call the appropriate setup function in your mode hooks:
;;
;;    ;; Python setup
;;    (add-hook 'python-mode-hook #'davidc-python-flymake-ruff-setup)
;;
;;    ;; C++ setup
;;    (add-hook 'c++-mode-hook #'davidc-flymake-clang-tidy-setup)
;;
;;    ;; Rust setup
;;    (add-hook 'rust-mode-hook #'davidc-flymake-rust-cargo-clippy-setup)
;;
;;    ;; JSON setup
;;    (add-hook 'js-json-mode-hook #'davidc-flymake-jsonc-setup)
;;
;;    ;; YAML setup
;;    (add-hook 'davidc-yaml-mode-hook #'davidc-flymake-yaml-setup)
;;
;;    ;; XML setup
;;    (add-hook 'xml-mode-hook #'davidc-flymake-xml-setup)
;;
;; NAVIGATION COMMANDS:
;; This package provides convenient functions to navigate between diagnostics:
;;
;;   M-x flymake-next-error     - Jump to next error
;;   M-x flymake-prev-error     - Jump to previous error
;;   M-x flymake-next-warning   - Jump to next warning
;;   M-x flymake-prev-warning   - Jump to previous warning
;;   M-x flymake-next-note      - Jump to next note/info
;;   M-x flymake-prev-note      - Jump to previous note/info
;;
;; CUSTOMIZATION:
;; You can customize tool paths and arguments by setting these variables
;; before loading this package (e.g., in your custom-vars.el):
;;
;;   ;; Custom tool paths (useful if tools aren't in PATH)
;;   (setq davidc-python-flymake-ruff-path "/usr/local/bin/ruff")
;;   (setq davidc-flymake-clang-tidy-path "/opt/llvm/bin/clang-tidy")
;;   (setq davidc-flymake-rust-cargo-path "/home/user/.cargo/bin/cargo")
;;   (setq davidc-flymake-jsonc-lint-path "/path/to/jsonc_lint.py")
;;   (setq davidc-flymake-yaml-lint-path "/path/to/yaml_lint.py")
;;   (setq davidc-flymake-xml-lint-path "/path/to/xml_lint.py")
;;
;;   ;; Custom Rust clippy arguments
;;   (setq davidc-flymake-rust-cargo-clippy-args
;;         '("-W" "clippy::all" "-W" "clippy::pedantic"))
;;
;; AUTOMATIC CHECKING:
;; To enable automatic checking on file save, add this to your configuration:
;;
;;   (add-hook 'after-save-hook #'davidc-flymake-on-save)
;;
;; TROUBLESHOOTING:
;; * If tools aren't found, check they're installed and in your PATH
;; * For Rust projects, make sure you're in a Cargo project directory.
;; * For C++, clang-tidy works best with "compile_commands.json".
;; * For JSON, make sure Python 3 and jsonc_lint.py are available.
;; * For YAML, make sure Python 3 and yaml_lint.py are available.
;; * For XML, make sure Python 3 and xml_lint.py are available.
;; * Use M-x flymake-log to see detailed diagnostic information.
;;

(require 'flymake)
(require 'cl-lib)

;; Utility function to get platform-specific executable path.
(defun davidc-flymake--get-executable-path (base-name)
  "Get the platform-specific executable path for BASE-NAME."
  (if (string-equal system-type "windows-nt")
      (concat base-name ".exe")
    base-name))

;; Define default settings if not already defined in 'custom-vars.el'.
(defvar davidc-python-flymake-ruff-path
  (davidc-flymake--get-executable-path "ruff")
  "Path to the ruff executable for Python linting.
Set this variable before loading this package to use a custom path.")

(defvar davidc-flymake-clang-tidy-path
  (davidc-flymake--get-executable-path "clang-tidy")
  "Path to the clang-tidy executable for C++ static analysis.
Set this variable before loading this package to use a custom path.")

(defvar davidc-flymake-rust-cargo-path
  (davidc-flymake--get-executable-path "cargo")
  "Path to the Rust cargo executable.
Set this variable before loading this package to use a custom path.")

(defvar davidc-flymake-jsonc-lint-path
  (expand-file-name "bin/jsonc_lint.py" user-emacs-directory)
  "Path to the JSONC linter script.
This should be a Python script that can lint JSON files with comments.
Defaults to ~/.emacs.d/bin/jsonc_lint.py.
Set this variable before loading this package to use a custom path.")

(defvar davidc-flymake-yaml-lint-path
  (expand-file-name "bin/yaml_lint.py" user-emacs-directory)
  "Path to the YAML linter script.")

(defvar davidc-flymake-xml-lint-path
  (expand-file-name "bin/xml_lint.py" user-emacs-directory)
  "Path to the XML linter script.
This should be a Python script that can lint XML files.
Defaults to ~/.emacs.d/bin/xml_lint.py.
Set this variable before loading this package to use a custom path.")

(defvar davidc-flymake-python-path
  (davidc-flymake--get-executable-path "python3")
  "Path to the Python 3 executable.
Used for running Python-based linters like the JSONC linter.
Set this variable before loading this package to use a custom path.")

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

;; Setup flymake configuration for Python with ruff.
(defun davidc-python-flymake-ruff-setup ()
  "Configure flymake for Python using ruff.
Call this function in python-mode-hook to enable automatic Python linting."
  (when (and (boundp 'python-flymake-command)
             (boundp 'python-flymake-command-output-pattern)
             (boundp 'python-flymake-msg-alist))
    ;; Use the settings from custom-vars.el if they exist, otherwise
    ;; use default values.
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

;; Generic save hook for flymake.
(defun davidc-flymake-on-save ()
  "Run Flymake only when buffer is saved.
Add this to after-save-hook to enable automatic checking on file save."
  (when flymake-mode
    (flymake-start)))

;; Common diagnostic parsing function.
(defun davidc-flymake--parse-diagnostics (source regex-pattern process-buffer current-file-matcher source-file)
  "Parse diagnostics from PROCESS-BUFFER using REGEX-PATTERN.
SOURCE is the source buffer.
CURRENT-FILE-MATCHER is a function that takes (file-path source-file) and returns
whether it matches the current file.
SOURCE-FILE is the path of the file being checked."
  (let ((diags '()))
    (with-current-buffer process-buffer
      ;; ;; Debug output - print the entire buffer content
      ;; (let ((output-content (buffer-string)))
      ;;   (message "[DEBUG] Output:\n%s" output-content))

      (goto-char (point-min))
      (while (re-search-forward regex-pattern nil t)
        (let* ((file-path (match-string 1))
               (line (string-to-number (match-string 2)))
               (col (string-to-number (match-string 3)))
               (type-str (match-string 4))
               (message-text (match-string 5))
               (level (cond
                       ((string-match-p "^error" type-str) :error)
                       ((string= type-str "warning") :warning)
                       (t :note))))

          ;; ;; Debug output for each match
          ;; (message "[DEBUG] Found: %s at %s:%d:%d - %s"
          ;;          type-str file-path line col message-text)

          (when (and (funcall current-file-matcher file-path source-file)
                     (not (eq level :note))) ; Skip notes for most tools
            (let ((beg-end (flymake-diag-region source line col)))
              (when beg-end
                ;; (message "[DEBUG] Adding diagnostic: %s at %d:%d"
                ;;          message-text line col)
                (push (flymake-make-diagnostic source
                                               (car beg-end)
                                               (cdr beg-end)
                                               level
                                               message-text)
                      diags)))))))
    diags))


;; Generic flymake backend factory.
(defun davidc-flymake--make-backend (tool-name executable-path command-builder regex-pattern file-matcher)
  "Create a flymake backend function.
TOOL-NAME is the name of the tool (for messages).
EXECUTABLE-PATH is the path to the executable.
COMMAND-BUILDER is a function that takes a source-file and returns the command list.
REGEX-PATTERN is the regex to parse output.
FILE-MATCHER is a function to determine if a diagnostic applies to current file."
  (let ((proc-var (intern (format "davidc--flymake-%s-proc" tool-name))))
    (lambda (report-fn &rest _args)
      ;; Check if executable exists
      (unless (executable-find executable-path)
        (error "Flymake mode %s; Cannot find executable at \"%s\"." tool-name executable-path))
      ;; (message "[DEBUG] Flymake mode %s; using executable at \"%s\"." tool-name executable-path)

      ;; Kill any existing process.
      (when (process-live-p (buffer-local-value proc-var (current-buffer)))
        (kill-process (buffer-local-value proc-var (current-buffer))))

      (let* ((source (current-buffer))
             (source-file (buffer-file-name)))

        (when source-file
          ;; When this runs from 'after-save-hook', the buffer is already
          ;; saved. If we need to ensure the file is saved, we should save
          ;; it ourselves rather than erroring out.
          (when (buffer-modified-p)
            (save-buffer))

          ;; (message "[DEBUG] Flymake mode %s; Source File: \"%s\"." tool-name source-file)
          (save-restriction
            (widen)

            ;; Create the process.
            (set (make-local-variable proc-var)
                 (make-process
                  :name (format "flymake-%s" tool-name)
                  :noquery t
                  :connection-type 'pipe
                  :buffer (generate-new-buffer (format " *flymake-%s*" tool-name))
                  :file-handler t
                  :command (funcall command-builder source-file)
                  :sentinel
                  (lambda (proc _event)
                    ;; Check that the process has indeed exited.
                    (when (memq (process-status proc) '(exit signal))
                      (unwind-protect
                          ;; Only proceed if proc is the current process.
                          (if (with-current-buffer source
                                (eq proc (buffer-local-value proc-var source)))
                              (let ((diags (davidc-flymake--parse-diagnostics
                                            source
                                            regex-pattern
                                            (process-buffer proc)
                                            file-matcher
                                            source-file)))
                                ;; Report the diagnostics.
                                (message "Flymake mode %s; Found %d diagnostics for \"%s\"."
                                         tool-name
                                         (length diags)
                                         (file-name-nondirectory source-file))
                                (funcall report-fn diags))
                            ;; If obsolete, log warning.
                            (flymake-log :warning "Canceling obsolete check %s" proc))
                        ;; Clean up resources.
                        (when (process-buffer proc)
                          (kill-buffer (process-buffer proc))))))))))))))

;; Local variables to keep track of processes.
(defvar-local davidc--flymake-clang-tidy-proc nil
  "Current clang-tidy flymake process.")

(defvar-local davidc--flymake-rust-cargo-clippy-proc nil
  "Current cargo clippy flymake process.")

(defvar-local davidc--flymake-jsonc-proc nil
  "Current JSONC lint flymake process.")

(defvar-local davidc--flymake-yaml-proc nil
  "Current YAML lint flymake process.")

(defvar-local davidc--flymake-xml-proc nil
  "Current XML lint flymake process.")

;; File matcher functions.
(defun davidc-flymake--exact-file-matcher (file-path source-file)
  "Match FILE-PATH against SOURCE-FILE exactly or by basename."
  (let* ((normalized-source (expand-file-name source-file))
         (normalized-file (expand-file-name (directory-file-name file-path))))
    (or (string= normalized-file normalized-source)
        (string-suffix-p (file-name-nondirectory source-file) file-path))))

(defun davidc-flymake--basename-file-matcher (file-path source-file)
  "Match FILE-PATH against SOURCE-FILE by basename only."
  (string= (file-name-nondirectory file-path)
           (file-name-nondirectory source-file)))

;; Create the actual backend functions.
(defalias 'davidc-flymake-clang-tidy
  (davidc-flymake--make-backend
   "clang-tidy"
   davidc-flymake-clang-tidy-path
   (lambda (source-file)
     (list davidc-flymake-clang-tidy-path
           "-quiet"
           (file-name-nondirectory source-file)))
   "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(warning\\|error\\|note\\): \\(.*\\)$"
   #'davidc-flymake--exact-file-matcher)
  "Flymake backend for clang-tidy.
REPORT-FN is the callback function for reporting diagnostics.")

(defalias 'davidc-flymake-rust-cargo-clippy
  (davidc-flymake--make-backend
   "rust-cargo-clippy"
   davidc-flymake-rust-cargo-path
   (lambda (_source-file)
     (append (list davidc-flymake-rust-cargo-path
                   "clippy"
                   "--message-format=short"
                   "--")
             davidc-flymake-rust-cargo-clippy-args))
   "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(warning\\|error\\(?:\\[E[0-9]+\\]\\)?\\): \\(.*\\)$"
   #'davidc-flymake--basename-file-matcher)
  "Flymake backend for Rust's clippy.
REPORT-FN is the callback function for reporting diagnostics.")

(defalias 'davidc-flymake-jsonc
  (davidc-flymake--make-backend
   "jsonc"
   davidc-flymake-python-path
   (lambda (source-file)
     (list davidc-flymake-python-path davidc-flymake-jsonc-lint-path source-file))
   "^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\): \\(error\\|warning\\|note\\): \\(.+\\)$"
   #'davidc-flymake--basename-file-matcher)
  "Flymake backend for JSONC (JSON with comments).
REPORT-FN is the callback function for reporting diagnostics.")

(defalias 'davidc-flymake-yaml
  (davidc-flymake--make-backend
   "yaml"
   davidc-flymake-python-path
   (lambda (source-file)
     (list davidc-flymake-python-path davidc-flymake-yaml-lint-path source-file))
   "^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\): \\(error\\|warning\\|note\\): \\(.+\\)$"
   #'davidc-flymake--basename-file-matcher)
  "Flymake backend for YAML files.")

(defalias 'davidc-flymake-xml
  (davidc-flymake--make-backend
   "xml"
   davidc-flymake-python-path
   (lambda (source-file)
     (list davidc-flymake-python-path davidc-flymake-xml-lint-path source-file))
   "^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\): \\(error\\|warning\\|note\\): \\(.+\\)$"
   #'davidc-flymake--basename-file-matcher)
  "Flymake backend for XML files.
REPORT-FN is the callback function for reporting diagnostics.")

;; Setup functions.
(defun davidc-flymake-clang-tidy-setup ()
  "Set up flymake for clang-tidy.
Call this function in c++-mode-hook to enable automatic C++ static analysis."
  (interactive)
  (message "Setting up clang-tidy for flymake.")
  (add-hook 'flymake-diagnostic-functions #'davidc-flymake-clang-tidy nil t)
  (flymake-mode 1))

(defun davidc-flymake-rust-cargo-clippy-setup ()
  "Set up flymake for rust clippy.
Call this function in rust-mode-hook to enable automatic Rust linting."
  (interactive)
  (message "Setting up Rust clippy for flymake.")
  (add-hook 'flymake-diagnostic-functions #'davidc-flymake-rust-cargo-clippy nil t)
  (flymake-mode 1))

(defun davidc-flymake-jsonc-setup ()
  "Set up flymake for JSONC files with comment support.
Call this function in js-json-mode-hook to enable automatic JSON linting."
  (interactive)
  (message "Setting up JSONC linter for flymake.")
  (add-hook 'flymake-diagnostic-functions #'davidc-flymake-jsonc nil t)
  (flymake-mode 1))

(defun davidc-flymake-yaml-setup ()
  "Set up flymake for YAML files."
  (interactive)
  (message "Setting up YAML linter for flymake.")
  (add-hook 'flymake-diagnostic-functions #'davidc-flymake-yaml nil t)
  (flymake-mode 1))

(defun davidc-flymake-xml-setup ()
  "Set up flymake for XML files.
Call this function in xml-mode-hook to enable automatic XML linting."
  (interactive)
  (message "Setting up XML linter for flymake.")
  (add-hook 'flymake-diagnostic-functions #'davidc-flymake-xml nil t)
  (flymake-mode 1))


;; Simplified diagnostic navigation.
(defun davidc-flymake-get-sorted-diagnostics (reverse)
  "Get all Flymake diagnostics sorted by position.
If REVERSE is non-nil, sort in reverse order."
  (let ((diagnostics (flymake-diagnostics)))
    (sort diagnostics
          (if reverse
              (lambda (a b) (> (flymake-diagnostic-beg a) (flymake-diagnostic-beg b)))
            (lambda (a b) (< (flymake-diagnostic-beg a) (flymake-diagnostic-beg b)))))))

(defun davidc-flymake--find-matching-diagnostic (diagnostics level direction current-pos)
  "Find the next/prev diagnostic matching LEVEL in DIRECTION from CURRENT-POS.
Returns (diagnostic . wrapped) where wrapped indicates if search wrapped around."
  (let* ((forward-p (eq direction 'next))
         (found nil)
         (wrapped nil))
    ;; First pass: search in the specified direction.
    (catch 'found
      (dolist (diag diagnostics)
        (when (and (eq (flymake-diagnostic-type diag) level)
                   (if forward-p
                       (> (flymake-diagnostic-beg diag) current-pos)
                     (< (flymake-diagnostic-beg diag) current-pos)))
          (setq found diag)
          (throw 'found t))))

    ;; If not found, wrap around and find any diagnostic of the right
    ;; level.
    (unless found
      (setq wrapped t)
      (setq found (seq-find (lambda (diag)
                              (eq (flymake-diagnostic-type diag) level))
                            diagnostics)))

    (cons found wrapped)))

(defun davidc-flymake-goto-diagnostic (direction &optional level)
  "Go to the next/previous Flymake diagnostic with specified LEVEL.
Will wrap around the buffer if no matching diagnostic is found.

DIRECTION can be 'next or 'prev.
LEVEL can be :error, :warning, or :note. If not specified, use :error."
  (interactive)
  (unless flymake-mode
    (user-error "Flymake not running in current buffer"))

  (let* ((level-value (or level :error))
         (level-name (symbol-name level-value))
         (diagnostics (davidc-flymake-get-sorted-diagnostics (eq direction 'prev))))

    (if (null diagnostics)
        (message "No diagnostics found in buffer")
      (let* ((result (davidc-flymake--find-matching-diagnostic
                      diagnostics level-value direction (point)))
             (found-diag (car result))
             (wrapped (cdr result)))

        (if found-diag
            (progn
              (goto-char (flymake-diagnostic-beg found-diag))
              (when wrapped
                (message "Reached %s of buffer, wrapped to %s and found %s %s"
                         (if (eq direction 'next) "end" "beginning")
                         (if (eq direction 'next) "beginning" "end")
                         (if (eq level-value :error) "an" "a")
                         level-name)))
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
