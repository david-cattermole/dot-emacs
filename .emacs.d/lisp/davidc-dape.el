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
;;   M-x davidc-dape-attach-to-process RET myprogram RET
;;   (then select debugger configuration)
;;
;; Launch Python program:
;;   M-x dape RET debugpy-davidc-launch RET
;;
;; Attach to Python with debugpy:
;;   1. In Python code: import debugpy; print("debugpy: listening on 5678..."); debugpy.listen(5678); debugpy.wait_for_client(); print("debugpy: connected!")
;;   2. M-x dape RET debugpy-davidc-attach RET

(require 'dape)

;;; Process Discovery and Selection.

(defvar davidc-dape--last-process-name nil
  "Cache for the last process name entered to avoid re-prompting.")

(defvar davidc-dape--last-processes nil
  "Cache for the last process list to avoid re-scanning.")

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
Uses caching to avoid re-prompting during dape configuration processing."
  (unless (and davidc-dape--last-process-name davidc-dape--last-processes)
    (let ((process-name (read-string "Process name to attach to: ")))
      (setq davidc-dape--last-process-name process-name)
      (setq davidc-dape--last-processes (davidc-dape-get-processes-by-name process-name))))

  (if davidc-dape--last-processes
      (if (= 1 (length davidc-dape--last-processes))
          (plist-get (car davidc-dape--last-processes) :pid)
        (let* ((choices (mapcar (lambda (proc)
                                  (let ((pid (plist-get proc :pid))
                                        (start-time (plist-get proc :start-time))
                                        (cmd (plist-get proc :full)))
                                    (cons (format "%s, %s, %d" cmd start-time pid) pid)))
                                davidc-dape--last-processes))
               (selected (completing-read
                         (format "Multiple '%s' processes found, select one: " davidc-dape--last-process-name)
                         choices nil t)))
          (prog1 (cdr (assoc selected choices))
            ;; Clear cache after selection.
            (setq davidc-dape--last-process-name nil
                  davidc-dape--last-processes nil))))
    (user-error "No running processes found matching '%s'" davidc-dape--last-process-name)))

;;; Dape Configuration Setup.
;;;
;;; https://github.com/microsoft/debugpy/wiki/Debug-configuration-settings
;;;

;;;###autoload
(defun davidc-dape-setup-attach-configs ()
  "Add attach configurations for C/C++ and Rust to dape-configs."

  ;; GDB attach configuration for C/C++.
  (add-to-list 'dape-configs
               `(gdb-attach-cpp
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
                 :pid ,(lambda () (davidc-dape-read-process-name-and-pid))))

  ;; CodeLLDB attach configuration for C/C++.
  (add-to-list 'dape-configs
               `(codelldb-attach-cpp
                 modes (c-mode c-ts-mode c++-mode c++-ts-mode)
                 ensure dape-ensure-command
                 command-cwd dape-command-cwd
                 command ,(file-name-concat dape-adapter-dir "codelldb" "extension" "adapter" "codelldb")
                 command-args ("--port" :autoport)
                 port :autoport
                 :type "lldb"
                 :request "attach"
                 :pid ,(lambda () (davidc-dape-read-process-name-and-pid))
                 :stopOnEntry nil))

  ;; CodeLLDB attach configuration for Rust.
  (add-to-list 'dape-configs
               `(codelldb-attach-rust
                 modes (rust-mode rust-ts-mode)
                 ensure dape-ensure-command
                 command-cwd dape-command-cwd
                 command ,(file-name-concat dape-adapter-dir "codelldb" "extension" "adapter" "codelldb")
                 command-args ("--port" :autoport "--settings" "{\"sourceLanguages\":[\"rust\"]}")
                 port :autoport
                 :type "lldb"
                 :request "attach"
                 :pid ,(lambda () (davidc-dape-read-process-name-and-pid))
                 :stopOnEntry nil))

  ;; GDB attach configuration for Rust.
  (add-to-list 'dape-configs
               `(gdb-attach-rust
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
                 :pid ,(lambda () (davidc-dape-read-process-name-and-pid))))

  ;; cpptools attach configuration for C/C++.
  (add-to-list 'dape-configs
               `(cpptools-attach
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
                 :MIMode ,(seq-find 'executable-find '("lldb" "gdb"))))

  ;; Python debugpy launch configuration.
  (add-to-list 'dape-configs
               `(debugpy-davidc-launch
                 modes (python-mode python-ts-mode)
                 ensure (lambda (config)
                          (dape-ensure-command config)
                          (let ((python (dape-config-get config 'command)))
                            (unless (zerop
                                     (call-process-shell-command
                                      (format "%s -c \"import debugpy.adapter\"" python)))
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
                 :cwd dape-cwd))

  ;; Python debugpy launch configuration for Maya 2024.
  (add-to-list 'dape-configs
               `(debugpy-maya2024-launch
                 modes (python-mode python-ts-mode)
                 ensure (lambda (config)
                          (dape-ensure-command config)
                          (let ((python (dape-config-get config 'command)))
                            (unless (zerop
                                     (call-process-shell-command
                                      (format "%s -c \"import debugpy.adapter\"" python)))
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
                 :cwd dape-cwd))

  ;; Python debugpy attach configuration.
  (add-to-list 'dape-configs
               `(debugpy-davidc-attach
                 modes (python-mode python-ts-mode)
                 host "localhost"
                 port 5678
                 :type "python"
                 :request "attach"
                 :console "integratedTerminal"
                 :showReturnValue t
                 :justMyCode nil
                 :cwd dape-cwd)))

(provide 'davidc-dape)
