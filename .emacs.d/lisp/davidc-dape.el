;;; -*- lexical-binding: t -*-
;;; Debug Adapter Protocol for Emacs attach configurations.
;;
;; This module provides enhanced dape (Debug Adapter Protocol for Emacs) configurations
;; for debugging C/C++, Rust, and Python programs with advanced process discovery features.
;;
;; Features:
;; - Process discovery by name with start time information
;; - Interactive process selection when multiple matches exist
;; - Support for GDB, CodeLLDB, cpptools, and debugpy debuggers
;; - Processes sorted by start time (newest first)
;; - Clean UX without double prompts
;; - Launch and attach configurations for Python (including Maya integration)
;;
;; Supported Debuggers:
;; - GDB (GNU Debugger) for C/C++ and Rust
;; - CodeLLDB (LLVM-based debugger) for C/C++ and Rust
;; - cpptools (Microsoft C++ extension) for C/C++
;; - debugpy (Python debugger) for Python applications and Maya
;;
;; Setup Instructions:
;;
;; 1. GDB Setup:
;;    - Requires GDB version >= 14.1 with DAP support
;;    - Install: sudo dnf install gdb (on RHEL/Alma/Rocky)
;;    - Install: sudo apt install gdb (on Ubuntu/Debian)
;;    - Verify: gdb --version
;;
;; 2. CodeLLDB Setup:
;;    - Download from: https://github.com/vadimcn/codelldb
;;    - Extract to: ~/.emacs.d/dape-adapter/codelldb/
;;    - Ensure codelldb binary is executable
;;
;; 3. cpptools Setup:
;;    - Download from: https://github.com/Microsoft/vscode-cpptools
;;    - Extract to: ~/.emacs.d/dape-adapter/cpptools/
;;    - Ensure OpenDebugAD7 binary is executable
;;
;; 4. debugpy Setup:
;;    - Install: pip install debugpy
;;    - For Maya: /usr/autodesk/maya2024/bin/mayapy -m pip install debugpy
;;
;; Usage Examples:
;;
;; Attach to running process:
;;   M-x davidc-dape-attach
;;
;; Launch Python program:
;;   M-x davidc-dape-launch

(require 'dape)

;;; Process Discovery and Selection.

(defvar davidc-dape--selected-pid nil
  "Stores the PID selected by the user for the current attach session.
Set by `davidc-dape-attach' before calling dape, used by the :pid lambda.")

(defun davidc-dape-convert-lstart-to-yyyy-mm-dd (lstart-string)
  "Convert ps lstart format to YYYY/MM/DD HH:MM:SS format.
Input format: 'Day Mon DD HH:MM:SS YYYY' (e.g., 'Mon Sep 15 12:10:22 2025')
Output format: 'YYYY/MM/DD HH:MM:SS' (e.g., '2025/09/15 12:10:22')"
  (when (string-match "^\\w+\\s-+\\(\\w+\\)\\s-+\\([0-9]+\\)\\s-+\\([0-9:]+\\)\\s-+\\([0-9]+\\)$" lstart-string)
    (let* ((month-name (match-string 1 lstart-string))
           (day (match-string 2 lstart-string))
           (time (match-string 3 lstart-string))
           (year (match-string 4 lstart-string))
           (month-alist '(("Jan" . "01") ("Feb" . "02") ("Mar" . "03") ("Apr" . "04")
                         ("May" . "05") ("Jun" . "06") ("Jul" . "07") ("Aug" . "08")
                         ("Sep" . "09") ("Oct" . "10") ("Nov" . "11") ("Dec" . "12")))
           (month-num (cdr (assoc month-name month-alist))))
      (when month-num
        (format "%s/%s/%02d %s" year month-num (string-to-number day) time)))))

(defun davidc-dape-get-processes-by-name (process-name)
  "Get list of process IDs and commands for PROCESS-NAME.
Returns a list of plists with :pid, :command, :args, :start-time, and :elapsed-seconds keys.
Processes are sorted by start time with newest first."
  ;; TODO: This code uses "ps" and "grep" commands for Linux, but this
  ;; does not work for Windows. We need to add support fori Microsoft Windows.
  (let ((ps-output (shell-command-to-string
                    (format "ps -eo pid,lstart,etimes,args --no-headers | grep -E '%s' | grep -v grep"
                            (regexp-quote process-name))))
        (processes '()))
    (dolist (line (split-string ps-output "\n" t "[ \t]+"))
      (when (string-match "^[ \t]*\\([0-9]+\\)[ \t]+\\([A-Za-z]+ [A-Za-z]+ [0-9]+ [0-9:]+ [0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(.+\\)$" line)
        (let* ((pid (string-to-number (match-string 1 line)))
               (lstart-raw (match-string 2 line))
               (elapsed-seconds (string-to-number (match-string 3 line)))
               (full-command (match-string 4 line))
               (start-time (davidc-dape-convert-lstart-to-yyyy-mm-dd lstart-raw)))
          (when (and (> pid 0)
                     (string-match-p (regexp-quote process-name) full-command)
                     start-time)
            (let* ((args (split-string full-command))
                   (command (car args)))
              (push (list :pid pid
                         :command command
                         :args args
                         :full full-command
                         :start-time start-time
                         :elapsed-seconds elapsed-seconds)
                    processes))))))

    ;; Sort by elapsed seconds (ascending = newest first).
    (sort processes (lambda (a b) (< (plist-get a :elapsed-seconds)
                                     (plist-get b :elapsed-seconds))))))

(defun davidc-dape-select-process-pid (process-name)
  "Interactively select a process PID for PROCESS-NAME.
If only one process matches, return its PID directly.
If multiple processes match, prompt user to select one.
If no processes match, signal an error."
  (let ((processes (davidc-dape-get-processes-by-name process-name)))
    (cond
     ((null processes)
      (user-error "No running processes found matching '%s'" process-name))
     ((= 1 (length processes))
      (plist-get (car processes) :pid))
     (t
      (let* ((choices (mapcar (lambda (proc)
                                (let ((pid (plist-get proc :pid))
                                      (start-time (plist-get proc :start-time))
                                      (cmd (plist-get proc :full)))
                                  (cons (format "%s, %s, %d" cmd start-time pid) pid)))
                              processes))
             (selected (completing-read
                       (format "Multiple '%s' processes found, select one: " process-name)
                       choices nil t)))
        (cdr (assoc selected choices)))))))

(defun davidc-dape-read-process-name-and-pid ()
  "Read process name and return selected PID.
First checks if a PID was already selected (stored in `davidc-dape--selected-pid').
If not, prompts for process selection."
  (cond
   ;; If PID already selected by davidc-dape-attach, use it
   (davidc-dape--selected-pid
    (message "[dape-debug] Using pre-selected PID: %d" davidc-dape--selected-pid)
    davidc-dape--selected-pid)

   ;; If we're in preview mode (building defaults), return nil to avoid prompting
   ((> (minibuffer-depth) 0)
    (message "[dape-debug] Skipping prompt (preview mode, depth=%d)" (minibuffer-depth))
    nil)

   ;; Otherwise, prompt for process name and PID
   (t
    (let* ((process-name (read-string "Process name to attach to: "))
           (processes (davidc-dape-get-processes-by-name process-name)))
      (message "[dape-debug] Looking for processes matching: %s" process-name)
      (message "[dape-debug] Found %d matching processes" (length processes))
      (cond
       ((null processes)
        (user-error "No running processes found matching '%s'" process-name))
       ((= 1 (length processes))
        (let ((pid (plist-get (car processes) :pid)))
          (message "[dape-debug] Single process found, using PID: %d" pid)
          pid))
       (t
        (let* ((choices (mapcar (lambda (proc)
                                  (let ((pid (plist-get proc :pid))
                                        (start-time (plist-get proc :start-time))
                                        (cmd (plist-get proc :full)))
                                    (cons (format "%s, %s, %d" cmd start-time pid) pid)))
                                processes))
               (selected (completing-read
                         (format "Multiple '%s' processes found, select one: " process-name)
                         choices nil t)))
          (let ((pid (cdr (assoc selected choices))))
            (message "[dape-debug] User selected PID: %d" pid)
            pid))))))))

;;; Dape Configuration Setup.
;;;
;;; https://github.com/microsoft/debugpy/wiki/Debug-configuration-settings
;;;

(defvar davidc-dape-custom-configs
  `((gdb-attach-cpp
     modes (c-mode c-ts-mode c++-mode c++-ts-mode)
     ensure (lambda (config)
              (dape-ensure-command config)
              (let* ((default-directory
                      (or (dape-config-get config 'command-cwd)
                          default-directory))
                     (command (dape-config-get config 'command))
                     (output (shell-command-to-string (format "%s --version" command)))
                     (version (save-match-data
                                (when (string-match "GNU gdb \\(?:(.*) \\)?\\([0-9.]+\\)" output)
                                  (string-to-number (match-string 1 output))))))
                (unless (>= version 14.1)
                  (user-error "Requires gdb version >= 14.1"))))
     command-cwd dape-command-cwd
     command "gdb"
     command-args ("--interpreter=dap")
     :request "attach"
     :pid ,(lambda () (davidc-dape-read-process-name-and-pid)))

    (codelldb-attach-cpp
     modes (c-mode c-ts-mode c++-mode c++-ts-mode)
     ensure dape-ensure-command
     command-cwd dape-command-cwd
     command ,(file-name-concat dape-adapter-dir "codelldb" "extension" "adapter" "codelldb")
     command-args ("--port" :autoport)
     port :autoport
     :type "lldb"
     :request "attach"
     :pid ,(lambda () (davidc-dape-read-process-name-and-pid))
     :stopOnEntry nil)

    (codelldb-attach-rust
     modes (rust-mode rust-ts-mode)
     ensure dape-ensure-command
     command-cwd dape-command-cwd
     command ,(file-name-concat dape-adapter-dir "codelldb" "extension" "adapter" "codelldb")
     command-args ("--port" :autoport "--settings" "{\"sourceLanguages\":[\"rust\"]}")
     port :autoport
     :type "lldb"
     :request "attach"
     :pid ,(lambda () (davidc-dape-read-process-name-and-pid))
     :stopOnEntry nil)

    (gdb-attach-rust
     modes (rust-mode rust-ts-mode)
     ensure (lambda (config)
              (dape-ensure-command config)
              (let* ((default-directory
                      (or (dape-config-get config 'command-cwd)
                          default-directory))
                     (command (dape-config-get config 'command))
                     (output (shell-command-to-string (format "%s --version" command)))
                     (version (save-match-data
                                (when (string-match "GNU gdb \\(?:(.*) \\)?\\([0-9.]+\\)" output)
                                  (string-to-number (match-string 1 output))))))
                (unless (>= version 14.1)
                  (user-error "Requires gdb version >= 14.1"))))
     command-cwd dape-command-cwd
     command "gdb"
     command-args ("--interpreter=dap")
     :request "attach"
     :pid ,(lambda () (davidc-dape-read-process-name-and-pid)))

    (cpptools-launch-rust
     modes (rust-mode rust-ts-mode)
     ensure dape-ensure-command
     command-cwd dape-command-cwd
     command ,(file-name-concat dape-adapter-dir
                                "cpptools"
                                "extension"
                                "debugAdapters"
                                "bin"
                                "OpenDebugAD7")
     :type "cppdbg"
     :request "launch"
     :program ,(lambda () (read-file-name "Path to executable: " dape-cwd))
     :MIMode ,(seq-find 'executable-find '("lldb" "gdb")))

    (cpptools-attach
     modes (c-mode c-ts-mode c++-mode c++-ts-mode)
     ensure dape-ensure-command
     command-cwd dape-command-cwd
     command ,(file-name-concat dape-adapter-dir
                                "cpptools"
                                "extension"
                                "debugAdapters"
                                "bin"
                                "OpenDebugAD7")
     :type "cppdbg"
     :request "attach"
     :processId ,(lambda () (davidc-dape-read-process-name-and-pid))
     :MIMode ,(seq-find 'executable-find '("lldb" "gdb")))

    (debugpy-davidc-launch
     modes (python-mode python-ts-mode)
     ensure (lambda (config)
              (dape-ensure-command config)
              (let ((python (dape-config-get config 'command)))
                (unless (zerop
                         (call-process-shell-command
                          (format "%s -c "import debugpy.adapter"" python)))
                  (user-error "%s module debugpy is not installed" python))))
     command "python3"
     command-args ("-m" "debugpy.adapter" "--host" "0.0.0.0" "--port" :autoport)
     port :autoport
     :type "python"
     :request "launch"
     :program dape-buffer-default
     :console "integratedTerminal"
     :showReturnValue t
     :justMyCode nil
     :cwd dape-cwd)

    (debugpy-maya2024-launch
     modes (python-mode python-ts-mode)
     ensure (lambda (config)
              (dape-ensure-command config)
              (let ((python (dape-config-get config 'command)))
                (unless (zerop
                         (call-process-shell-command
                          (format "%s -c "import debugpy.adapter"" python)))
                  (user-error "%s module debugpy is not installed" python))))
     command "/usr/autodesk/maya2024/bin/mayapy"
     command-args ("-m" "debugpy.adapter" "--host" "0.0.0.0" "--port" :autoport)
     port :autoport
     :type "python"
     :request "launch"
     :program dape-buffer-default
     :console "integratedTerminal"
     :showReturnValue t
     :justMyCode nil
     :cwd dape-cwd)

    (debugpy-davidc-attach
     modes (python-mode python-ts-mode)
     host "localhost"
     port 5678
     :type "python"
     :request "attach"
     :console "integratedTerminal"
     :showReturnValue t
     :justMyCode nil
     :cwd dape-cwd))
  "Custom dape configurations for attach and launch actions.")

;;;###autoload
(defun davidc-dape-attach ()
  "Run dape with only attach configurations.
Prompts for process name/PID first, then shows available attach configurations."
  (interactive)
  (condition-case err
      (progn
        (message "[dape-debug] Filtering configs for attach...")
        (setq-local dape-configs
                    (seq-filter (lambda (c) (string= (plist-get (cdr c) :request) "attach"))
                                dape-configs))
        (message "[dape-debug] Found %d attach configs" (length dape-configs))

        ;; Prompt for process name and PID first
        (let ((process-name (read-string "Process name to attach to: ")))
          (message "[dape-debug] Prompting for process to attach...")
          (setq davidc-dape--selected-pid (davidc-dape-select-process-pid process-name))
          (message "[dape-debug] Selected PID: %d" davidc-dape--selected-pid))

        ;; Now call dape with the PID already selected
        (unwind-protect
            (call-interactively #'dape)
          ;; Always clear the selected PID after dape finishes
          (setq davidc-dape--selected-pid nil)
          (message "[dape-debug] Cleared selected PID")))
    (error
     ;; Make sure to clear the PID even on error
     (setq davidc-dape--selected-pid nil)
     (message "[dape-debug] Error in davidc-dape-attach: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

;;;###autoload
(defun davidc-dape-launch ()
  "Run dape with only launch configurations."
  (interactive)
  (condition-case err
      (progn
        (message "[dape-debug] Filtering configs for launch...")
        (setq-local dape-configs
                    (seq-filter (lambda (c) (string= (plist-get (cdr c) :request) "launch"))
                                dape-configs))
        (message "[dape-debug] Found %d launch configs" (length dape-configs))
        (call-interactively #'dape))
    (error
     (message "[dape-debug] Error in davidc-dape-launch: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

;;; Debugging Functions

;;;###autoload
(defun davidc-dape-debug-ps-output ()
  "Show raw ps command output for debugging.
This helps diagnose why process discovery might be failing."
  (interactive)
  (let* ((process-name (read-string "Process name to test: "))
         (ps-cmd (format "ps -eo pid,lstart,etimes,args --no-headers | grep -E '%s' | grep -v grep"
                        (regexp-quote process-name)))
         (ps-output (shell-command-to-string ps-cmd)))
    (with-current-buffer (get-buffer-create "*dape-ps-debug*")
      (erase-buffer)
      (insert (format "Process name: %s\n" process-name))
      (insert (format "Command: %s\n\n" ps-cmd))
      (insert "=== RAW PS OUTPUT ===\n")
      (insert ps-output)
      (insert "\n\n=== SPLIT LINES ===\n")
      (let ((lines (split-string ps-output "\n" t "[ \t]+")))
        (insert (format "Number of lines: %d\n\n" (length lines)))
        (dolist (line lines)
          (insert (format "Line: |%s|\n" line))
          (insert (format "  Length: %d chars\n" (length line)))
          (when (string-match "^[ \t]*\\([0-9]+\\)[ \t]+\\([A-Za-z]+ [A-Za-z]+ [0-9]+ [0-9:]+ [0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(.+\\)$" line)
            (insert (format "    MATCHES REGEX\n"))
            (insert (format "    PID: %s\n" (match-string 1 line)))
            (insert (format "    LSTART: %s\n" (match-string 2 line)))
            (insert (format "    ETIMES: %s\n" (match-string 3 line)))
            (insert (format "    ARGS: %s\n" (match-string 4 line))))
          (insert "\n")))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun davidc-dape-debug-process-discovery ()
  "Test process discovery and display results.
This helps debug issues with finding and selecting processes to attach to."
  (interactive)
  (let ((process-name (read-string "Process name to test: ")))
    (message "[dape-debug] Searching for processes matching: %s" process-name)
    (let ((processes (davidc-dape-get-processes-by-name process-name)))
      (message "[dape-debug] Found %d processes:" (length processes))
      (if (null processes)
          (message "[dape-debug] No processes found!")
        (dolist (proc processes)
          (message "[dape-debug]   PID: %d, Started: %s, Command: %s"
                   (plist-get proc :pid)
                   (plist-get proc :start-time)
                   (plist-get proc :full))))
      (with-current-buffer (get-buffer-create "*dape-process-discovery*")
        (erase-buffer)
        (insert (format "Process Discovery Test for: %s\n" process-name))
        (insert (format "Found %d matching processes\n\n" (length processes)))
        (if (null processes)
            (insert "No processes found matching this name.\n")
          (dolist (proc processes)
            (insert (format "PID: %d\n" (plist-get proc :pid)))
            (insert (format "  Command: %s\n" (plist-get proc :command)))
            (insert (format "  Full: %s\n" (plist-get proc :full)))
            (insert (format "  Start Time: %s\n" (plist-get proc :start-time)))
            (insert (format "  Elapsed: %d seconds\n\n" (plist-get proc :elapsed-seconds)))))
        (display-buffer (current-buffer))))))

;;;###autoload
(defun davidc-dape-show-configs ()
  "Show all available dape configurations.
Displays which configs match the current buffer's major mode."
  (interactive)
  (let ((attach-configs '())
        (launch-configs '())
        (other-configs '()))
    (dolist (entry dape-configs)
      (let* ((name (car entry))
             (config (cdr entry))
             (request (plist-get config :request))
             (modes (plist-get config 'modes))
             (mode-match (or (not modes)
                           (apply #'provided-mode-derived-p major-mode
                                  (cl-map 'list 'identity modes)))))
        (cond
         ((string= request "attach")
          (push (list name mode-match modes) attach-configs))
         ((string= request "launch")
          (push (list name mode-match modes) launch-configs))
         (t
          (push (list name mode-match modes) other-configs)))))
    (with-current-buffer (get-buffer-create "*dape-configs*")
      (erase-buffer)
      (insert (format "Current major mode: %s\n\n" major-mode))
      (insert "=== ATTACH CONFIGURATIONS ===\n\n")
      (dolist (item (reverse attach-configs))
        (let ((name (car item))
              (matches (cadr item))
              (modes (caddr item)))
          (insert (format "%s %s\n"
                          (if matches "Yes" "No")
                          name))
          (when modes
            (insert (format "  Modes: %s\n" modes)))
          (insert "\n")))
      (insert "\n=== LAUNCH CONFIGURATIONS ===\n\n")
      (dolist (item (reverse launch-configs))
        (let ((name (car item))
              (matches (cadr item))
              (modes (caddr item)))
          (insert (format "%s %s\n"
                          (if matches "Yes" "No")
                          name))
          (when modes
            (insert (format "  Modes: %s\n" modes)))
          (insert "\n")))
      (display-buffer (current-buffer)))))

(provide 'davidc-dape)
