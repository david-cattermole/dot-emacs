;;; -*- lexical-binding: t -*-
;;;
;;; Find-based file search with GNU find and fd support.
;;
;; This package provides a unified interface for file searching using either
;; GNU find or the modern fd command.  It offers smart pattern matching,
;; multiple display formats, and integration with Emacs' compilation mode
;; for navigating results.
;;
;; FEATURES:
;;
;; * Dual backend support: Traditional GNU find or modern fd.
;; * Smart pattern processing with automatic wildcarding.
;; * Multiple display formats (absolute/relative paths, ls-style, basename only).
;; * Specialized search functions for common tasks.
;; * Case-sensitive/insensitive searching.
;; * Integration with grep for content searching.
;; * Project-aware searching.
;; * Full history support for patterns and directories.
;;
;; BASIC USAGE:
;;
;; Search for files by name pattern:
;;   M-x davidc-find-files RET pattern RET directory RET
;;
;;   Pattern examples:
;;   - ""          → Find all files
;;   - "test"      → Find files containing "test" (auto-wrapped as *test*)
;;   - "*.py"      → Find Python files (wildcards preserved)
;;   - "test_*.py" → Find Python test files
;;
;; Find files by various criteria:
;;   M-x davidc-find-large-files      - Files larger than specified size
;;   M-x davidc-find-recent-files     - Files modified recently
;;   M-x davidc-find-old-files        - Files not modified recently
;;   M-x davidc-find-empty-files      - Empty files
;;   M-x davidc-find-executable-files - Executable files
;;
;; CONFIGURATION:
;;
;; You can also override the backend ("find" or "fd"):
;;   M-x davidc-find-set-backend RET find
;;
;; Or override the display format ("abspath", "relative", "basename" or "ls"):
;;   M-x davidc-find-set-display-format RET ls
;;
;; Choose your default backend (find or fd):
;;   (setq davidc-find-backend 'fd)  ; Use fd (faster, fewer features)
;;   (setq davidc-find-backend 'find) ; Use find (more features, slower)
;;
;; Set display format:
;;   (setq davidc-find-display-format 'relative) ; or 'abspath, 'ls, 'basename
;;
;; Set default search directory:
;;   (setq davidc-find-default-directory "~/projects")
;;
;; NAVIGATION:
;;
;; Results appear in a compilation buffer where you can:
;; * g to refresh the search.
;; * q to close the results buffer.
;;
;; BACKEND DIFFERENCES:
;;
;; GNU find:
;; * More mature, available everywhere.
;; * Complex boolean expressions.
;; * POSIX compliant.
;;
;; fd:
;; * Much faster, especially on large directories.
;; * Respects .gitignore by default.
;; * Simpler syntax.
;; * Better Unicode support.
;; * Requires separate installation.


(require 'compile)
(require 'grep)
(require 'thingatpt)

;;; Utility functions.
(defun davidc-find--get-executable-path (base-name)
  "Get the platform-specific executable path for BASE-NAME.
On Windows, appends .exe extension.  On other platforms, returns BASE-NAME as-is."
  (if (string-equal system-type "windows-nt")
      (concat base-name ".exe")
    base-name))


;;; Customization.
(defgroup davidc-find nil
  "Find-based file search interface supporting multiple backends."
  :group 'davidc-config
  :prefix "davidc-find-")

(defcustom davidc-find-backend 'find
  "Backend to use for file searching.
The backend determines which command-line tool is used:

`find': Use traditional GNU find command
  - More features (complex expressions)
  - Available on all Unix-like systems
  - Slower on large directory trees.

`fd': Use modern fd command (https://github.com/sharkdp/fd)
  - Significantly faster performance.
  - Respects .gitignore files by default.
  - Simpler syntax.
  - Requires separate installation,"
  :type '(choice (const :tag "GNU find (traditional)" find)
                 (const :tag "fd (modern, faster)" fd))
  :group 'davidc-find)

(defcustom davidc-find-display-format 'abspath
  "Display format for search results in the compilation buffer.

`abspath': Show absolute paths (default).
  Example: /home/user/projects/file.txt

`ls': Show ls -l style output with permissions, size, date.
  Example: -rw-r--r-- 1 user group 1234 Jan 1 12:00 file.txt

`relative': Show paths relative to search directory.
  Example: src/main.cpp (when searching in /home/user/projects)

`basename': Show only the filename without directory.
  Example: file.txt"
  :type '(choice (const :tag "Absolute paths" abspath)
                 (const :tag "ls -l format (detailed)" ls)
                 (const :tag "Relative to search directory" relative)
                 (const :tag "Filename only" basename))
  :group 'davidc-find)

(defcustom davidc-find-executable
  (davidc-find--get-executable-path "find")
  "Path to the GNU find executable.
Customize this if find is not in your PATH or you want to use
a specific version."
  :type 'string
  :group 'davidc-find)

(defcustom davidc-find-fd-executable
  (davidc-find--get-executable-path "fd")
  "Path to the fd executable.
Customize this if fd is not in your PATH or you want to use
a specific version.  fd must be installed separately from:
https://github.com/sharkdp/fd"
  :type 'string
  :group 'davidc-find)

(defcustom davidc-find-find-default-args
  nil
  "Default arguments to pass to GNU find before search criteria.
These arguments are prepended to every find command.
Example: (\"-L\") to follow symbolic links.
Only used when `davidc-find-backend' is \\='find."
  :type '(choice (const :tag "No default arguments" nil)
                 (repeat :tag "Argument list" string))
  :group 'davidc-find)

(defcustom davidc-find-fd-default-args
  nil
  "Default arguments to pass to fd before search criteria.
These arguments are prepended to every fd command.
Example: (\"--hidden\") to include hidden files.
Example: (\"--no-ignore\") to not respect .gitignore files.
Only used when `davidc-find-backend' is \\='fd."
  :type '(choice (const :tag "No default arguments" nil)
                 (repeat :tag "Argument list" string))
  :group 'davidc-find)

(defcustom davidc-find-default-directory nil
  "Default directory for find operations.
If nil, uses the current buffer's directory.
Can be a string path or a function that returns a path."
  :type '(choice (const :tag "Use current directory" nil)
                 (directory :tag "Fixed directory")
                 (function :tag "Function returning directory"))
  :group 'davidc-find)

(defcustom davidc-find-ignore-case t
  "Whether to ignore case in name searches by default.
When non-nil, file name patterns are matched case-insensitively.
Individual commands may override this setting."
  :type 'boolean
  :group 'davidc-find)

;;; History variables.
(defvar davidc-find-history nil
  "History list for find search patterns.
Stores previously used search patterns for easy recall.")

(defvar davidc-find-directory-history nil
  "History list for find directories.
Stores previously searched directories.")

(defvar davidc-find-file-pattern-history nil
  "History list for file pattern wildcards.
Used specifically for file pattern filters in content searches.")


;;; Smart default helpers.
(defun davidc-find--default-file-pattern ()
  "Get a default file pattern based on current buffer's file type.
Returns a wildcard pattern like \\='*.py\\=' if in a Python file,
or nil if the current buffer has no file extension."
  (when buffer-file-name
    (let ((ext (file-name-extension buffer-file-name)))
      (when ext
        (concat "*." ext)))))

(defun davidc-find--default-search-string ()
  "Get a default search string based on current context.
Returns the symbol at point if available, or nil.
This provides a sensible default when searching for files
or content."
  (let ((sym (thing-at-point 'symbol t)))
    (when (and sym (not (string-blank-p sym)))
      sym)))

(defun davidc-find--read-string-with-default (prompt default history)
  "Read a string with DEFAULT shown in PROMPT and using HISTORY.
If DEFAULT is non-nil, it's shown in the prompt and returned
if the user enters an empty string."
  (read-string
   (if default
       (format "%s (default %s): " prompt default)
     (concat prompt ": "))
   nil
   history
   default))

(defun davidc-find--read-directory-with-default (prompt default)
  "Read directory with DEFAULT pre-filled in minibuffer.

This mimics rgrep's behavior where the directory is pre-filled,
allowing users to easily edit the path rather than starting from scratch."
  (read-directory-name prompt default default nil))


;;; Pattern processing.
(defun davidc-find--process-pattern (pattern)
  "Process PATTERN to add wildcards if needed.
This function implements smart pattern processing:

- Empty pattern or whitespace → nil (search all files)
- Pattern with wildcards (* or ?) → use as-is
- Pattern without wildcards → wrap with * (*pattern*)

Examples:
  \"\" → nil (find all)
  \"test\" → \"*test*\"
  \"*.py\" → \"*.py\"
  \"test_*.py\" → \"test_*.py\""
  (cond
   ;; Empty pattern - search all.
   ((or (null pattern) (string-empty-p (string-trim pattern)))
    nil)
   ;; Already has wildcards - use as-is.
   ((string-match-p "[*?]" pattern)
    pattern)
   ;; No wildcards - add them automatically for substring matching.
   (t
    (format "*%s*" pattern))))


;;; Display format handlers.
(defun davidc-find--get-find-display-args (display-format directory)
  "Get GNU find arguments for DISPLAY-FORMAT.
DIRECTORY is used for calculating relative paths.
Returns appropriate find command arguments as a string."
  (pcase display-format
    ('abspath "")  ; Default find behavior shows absolute paths.
    ('ls "-ls")    ; Detailed listing format.
    ('relative (format "-printf \"%%P\\n\""))  ; Path relative to start.
    ('basename "-printf \"%f\\n\"")           ; Filename only.
    (_ "")))

(defun davidc-find--get-fd-display-args (display-format directory)
  "Get fd arguments for DISPLAY-FORMAT.
DIRECTORY is used for calculating relative paths.
Returns appropriate fd command arguments as a string."
  (pcase display-format
    ('abspath "-a")         ; Force absolute paths.
    ('ls "--list-details")  ; Detailed listing (similar to ls -l).
    ('relative "")          ; fd shows relative paths by default.
    ('basename "-f")        ; Show only filenames without paths.
    (_ "")))

;;; Command building functions.
(defun davidc-find--build-find-command (directory pattern type-filter case-sensitive extra-args)
  "Build complete GNU find command string.

DIRECTORY: The search directory (will be expanded to absolute path)
PATTERN: Name pattern for filtering (nil for no pattern filter)
TYPE-FILTER: File type filter symbol:
  \\='file          → regular files only
  \\='directory     → directories only
  \\='executable    → executable files
  \\='empty-file    → empty regular files
  \\='empty-directory → empty directories
CASE-SENSITIVE: Whether pattern matching is case sensitive
EXTRA-ARGS: Additional find arguments as a string

Returns a complete find command ready for execution."
  (let* ((processed-pattern (davidc-find--process-pattern pattern))
         (expanded-directory (expand-file-name directory))
         (default-args-str (if davidc-find-find-default-args
                               (mapconcat #'shell-quote-argument
                                          davidc-find-find-default-args " ")
                             ""))
         (type-arg (pcase type-filter
                     ('file "-type f")
                     ('directory "-type d")
                     ('executable "-type f -executable")
                     ('empty-file "-type f -empty")
                     ('empty-directory "-type d -empty")
                     (_ "")))
         (pattern-arg (if processed-pattern
                          (format "%s \"%s\""
                                  (if case-sensitive "-name" "-iname")
                                  processed-pattern)
                        ""))
         (display-args (davidc-find--get-find-display-args
                        davidc-find-display-format expanded-directory))
         (parts (list davidc-find-executable
                      (shell-quote-argument expanded-directory)
                      default-args-str
                      type-arg
                      pattern-arg
                      (or extra-args "")
                      display-args)))
    ;; Join non-empty parts with spaces.
    (mapconcat #'identity (seq-filter (lambda (s) (not (string-empty-p s))) parts) " ")))

(defun davidc-find--build-fd-command (directory pattern type-filter case-sensitive extra-args)
  "Build complete fd command string.

DIRECTORY: The search directory (will be expanded to absolute path)
PATTERN: Name pattern for filtering (nil to match all files)
TYPE-FILTER: File type filter symbol:
  \\='file          → regular files only
  \\='directory     → directories only
  \\='executable    → executable files
  \\='empty-file    → empty files
  \\='empty-directory → empty directories
CASE-SENSITIVE: Whether pattern matching is case sensitive
EXTRA-ARGS: Additional fd arguments as a string

Returns a complete fd command ready for execution."
  (let* ((processed-pattern (davidc-find--process-pattern pattern))
         (expanded-directory (expand-file-name directory))
         (display-args (davidc-find--get-fd-display-args
                        davidc-find-display-format expanded-directory))
         ;; Handle --list-details specially to avoid duplicates.
         (all-default-args (append davidc-find-fd-default-args
                                   (when (and (not (member "--list-details" davidc-find-fd-default-args))
                                              (string-match-p "--list-details" display-args))
                                     '("--list-details"))))
         (default-args-str (if (and all-default-args
                                    (not (string-match-p "--list-details" display-args)))
                               (mapconcat #'shell-quote-argument
                                          all-default-args " ")
                             ""))
         (type-arg (pcase type-filter
                     ('file "-t f")
                     ('directory "-t d")
                     ('executable "-t x")
                     ('empty-file "-t e")
                     ('empty-directory "-t e -t d")
                     (_ "")))
         (case-arg (if case-sensitive "-s" "-i"))
         ;; Build command differently based on whether we have a pattern.
         (parts (if processed-pattern
                    ;; With pattern: use --glob for wildcard matching.
                    (list davidc-find-fd-executable
                          default-args-str
                          display-args
                          case-arg
                          type-arg
                          (or extra-args "")
                          "--glob"
                          (format "\"%s\"" processed-pattern)
                          (shell-quote-argument expanded-directory))
                  ;; Without pattern: match all files with ".".
                  (list davidc-find-fd-executable
                        default-args-str
                        display-args
                        case-arg
                        type-arg
                        (or extra-args "")
                        "."  ; Match all files.
                        (shell-quote-argument expanded-directory)))))
    ;; Join non-empty parts with spaces.
    (mapconcat #'identity (seq-filter (lambda (s) (not (string-empty-p s))) parts) " ")))


;;; Core execution function.
(defun davidc-find--run-command (command description)
  "Execute COMMAND and display results in a compilation buffer.
DESCRIPTION is used for the buffer name (*Find DESCRIPTION*)
and user messages.

The results buffer uses `grep-mode' for navigation, allowing:
- RET or mouse clicks to visit files
- n/p to move between results
- Highlighting of matches"
  (let ((buffer-name (format "*Find %s*" description)))
    ;; Set up compilation environment.
    (let ((compilation-buffer-name-function (lambda (_) buffer-name))
          (compilation-process-setup-function
           (lambda ()
             (setq-local compilation-disable-input t))))

      ;; Start the compilation process.
      (compilation-start command 'grep-mode nil
                         (lambda (_) buffer-name)))

    ;; Inform user about the search.
    (message "Running %s search (%s format): %s..."
             (if (eq davidc-find-backend 'fd) "fd" "find")
             davidc-find-display-format
             description)))


;;; Main search dispatcher.
(defun davidc-find--search (directory pattern type-filter case-sensitive extra-args description)
  "Dispatch search to appropriate backend and execute.

This is the main internal function that:
1. Determines which backend to use
2. Builds the appropriate command
3. Executes it via `davidc-find--run-command'

DIRECTORY: Search directory
PATTERN: Search pattern (may be nil)
TYPE-FILTER: File type filter symbol
CASE-SENSITIVE: Whether to use case-sensitive matching
EXTRA-ARGS: Additional backend-specific arguments
DESCRIPTION: Human-readable description for the results buffer"
  (let* ((executable (if (eq davidc-find-backend 'fd)
                         davidc-find-fd-executable
                       davidc-find-executable))
         (command (if (eq davidc-find-backend 'fd)
                      (davidc-find--build-fd-command directory pattern
                                                     type-filter case-sensitive extra-args)
                    (davidc-find--build-find-command directory pattern
                                                     type-filter case-sensitive extra-args))))

    ;; Check if executable exists.
    (unless (executable-find executable)
      (error "Cannot find executable: %s. Please install it or customize `%s'"
             executable
             (if (eq davidc-find-backend 'fd)
                 "davidc-find-fd-executable"
               "davidc-find-executable")))

    ;; Run the command.
    (davidc-find--run-command command description)))

;;; Interactive commands.

;;;###autoload
(defun davidc-find-files (pattern directory)
  "Find files matching PATTERN in DIRECTORY.

PATTERN is intelligently processed:
  \"\" or empty    → Find all files
  \"name\"         → Find files containing \\='name\\=' (becomes *name*)
  \"name*\"        → Find files starting with \\='name\\='
  \"*.py\"         → Find Python files
  \"test_*.py\"    → Find Python test files

The search uses the backend specified by `davidc-find-backend'.
Results are displayed in a compilation buffer where you can
navigate to files by pressing RET."
  (interactive
   (let* ((default-pattern (davidc-find--default-search-string))
          (pattern (davidc-find--read-string-with-default
                    "Find files matching pattern"
                    default-pattern
                    'davidc-find-history))
          (default-dir (or davidc-find-default-directory
                          default-directory
                          "."))
          (directory (davidc-find--read-directory-with-default
                      (if pattern
                          (format "Find files matching \"%s\" in directory" pattern)
                        "Find files in directory")
                      default-dir)))
     (list pattern directory)))

  (let* ((processed-pattern (davidc-find--process-pattern pattern))
         (description (if processed-pattern
                          (format "files matching '%s'" processed-pattern)
                        "all files")))
    (davidc-find--search directory pattern 'file
                         (not davidc-find-ignore-case) nil description)))

;;;###autoload
(defun davidc-find-directories (pattern directory)
  "Find directories matching PATTERN in DIRECTORY.

PATTERN is processed the same way as `davidc-find-files'.
Only directories are returned in the results."
  (interactive
   (let* ((default-pattern (davidc-find--default-search-string))
          (pattern (davidc-find--read-string-with-default
                    "Find directories matching pattern"
                    default-pattern
                    'davidc-find-history))
          (default-dir (or davidc-find-default-directory
                          default-directory
                          "."))
          (directory (davidc-find--read-directory-with-default
                      (if pattern
                          (format "Find directories matching \"%s\" in directory" pattern)
                        "Find directories in directory")
                      default-dir)))
     (list pattern directory)))

  (let* ((processed-pattern (davidc-find--process-pattern pattern))
         (description (if processed-pattern
                          (format "directories matching '%s'" processed-pattern)
                        "all directories")))
    (davidc-find--search directory pattern 'directory
                         (not davidc-find-ignore-case) nil description)))

;;;###autoload
(defun davidc-find-large-files (size directory)
  "Find files larger than SIZE in DIRECTORY.

SIZE should be in find/fd format:
- Number with suffix: 10M, 1G, 500k
- M = megabytes, G = gigabytes, k = kilobytes

Example: \"10M\" finds files larger than 10 megabytes."
  (interactive
   (let* ((size (read-string "Find files larger than (e.g., 10M, 1G): "
                            "10M" 'davidc-find-history))
          (default-dir (or davidc-find-default-directory
                          default-directory
                          "."))
          (directory (davidc-find--read-directory-with-default
                      (format "Find files larger than %s in directory" size)
                      default-dir)))
     (list size directory)))

  (let ((extra-args (if (eq davidc-find-backend 'fd)
                        (format "-S +%s" size)
                      (format "-size +%s" size))))
    (davidc-find--search directory nil 'file t extra-args
                         (format "files larger than %s" size))))

;;;###autoload
(defun davidc-find-recent-files (days directory)
  "Find files modified within the last DAYS in DIRECTORY.

DAYS is the number of days to look back.
Only files modified more recently than DAYS ago are shown."
  (interactive
   (let* ((days (read-number "Find files modified in last N days: " 7))
          (default-dir (or davidc-find-default-directory
                          default-directory
                          "."))
          (directory (davidc-find--read-directory-with-default
                      (format "Find files modified in last %d days in directory" days)
                      default-dir)))
     (list days directory)))

  (let ((extra-args (if (eq davidc-find-backend 'fd)
                        (format "--changed-within %dd" days)
                      (format "-mtime -%d" days))))
    (davidc-find--search directory nil 'file t extra-args
                         (format "files modified in last %d days" days))))

;;;###autoload
(defun davidc-find-old-files (days directory)
  "Find files not modified for more than DAYS in DIRECTORY.

DAYS is the number of days of inactivity.
Only files that haven't been modified for at least DAYS are shown."
  (interactive
   (let* ((days (read-number "Find files older than N days: " 30))
          (default-dir (or davidc-find-default-directory
                          default-directory
                          "."))
          (directory (davidc-find--read-directory-with-default
                      (format "Find files older than %d days in directory" days)
                      default-dir)))
     (list days directory)))

  (let ((extra-args (if (eq davidc-find-backend 'fd)
                        (format "--changed-before %dd" days)
                      (format "-mtime +%d" days))))
    (davidc-find--search directory nil 'file t extra-args
                         (format "files older than %d days" days))))

;;;###autoload
(defun davidc-find-empty-files (directory)
  "Find empty files in DIRECTORY.

Empty files have zero bytes of content.
Useful for finding placeholder files or cleaning up."
  (interactive
   (let ((default-dir (or davidc-find-default-directory
                         default-directory
                         ".")))
     (list (davidc-find--read-directory-with-default
            "Find empty files in directory"
            default-dir))))

  (davidc-find--search directory nil 'empty-file t nil "empty files"))

;;;###autoload
(defun davidc-find-empty-directories (directory)
  "Find empty directories in DIRECTORY.

Empty directories contain no files or subdirectories.
Useful for cleanup operations."
  (interactive
   (let ((default-dir (or davidc-find-default-directory
                         default-directory
                         ".")))
     (list (davidc-find--read-directory-with-default
            "Find empty directories in directory"
            default-dir))))

  (davidc-find--search directory nil 'empty-directory t nil "empty directories"))

;;;###autoload
(defun davidc-find-executable-files (directory)
  "Find executable files in DIRECTORY.

On Unix-like systems, finds files with executable permission.
Useful for finding scripts and binaries."
  (interactive
   (let ((default-dir (or davidc-find-default-directory
                         default-directory
                         ".")))
     (list (davidc-find--read-directory-with-default
            "Find executable files in directory"
            default-dir))))

  (davidc-find--search directory nil 'executable t nil "executable files"))

;;;###autoload
(defun davidc-find-by-owner (owner directory)
  "Find files owned by OWNER in DIRECTORY.

OWNER should be a valid username on the system.
The default is the current user."
  (interactive
   (let* ((owner (davidc-find--read-string-with-default
                  "Find files owned by user"
                  (user-login-name)
                  'davidc-find-history))
          (default-dir (or davidc-find-default-directory
                          default-directory
                          "."))
          (directory (davidc-find--read-directory-with-default
                      (format "Find files owned by %s in directory" owner)
                      default-dir)))
     (list owner directory)))

  (let ((extra-args (if (eq davidc-find-backend 'fd)
                        (format "-o %s" owner)
                      (format "-user %s" owner))))
    (davidc-find--search directory nil 'file t extra-args
                         (format "files owned by %s" owner))))

;;;###autoload
(defun davidc-find-project-files (pattern)
  "Find files matching PATTERN in the current project root.

Uses Emacs' project.el to determine the project root.
If not in a project, searches from the current directory.

This is a convenience wrapper around `davidc-find-files'
that automatically uses the project root as the search directory."
  (interactive
   (let ((default-pattern (davidc-find--default-search-string)))
     (list (davidc-find--read-string-with-default
            "Find files in project matching"
            default-pattern
            'davidc-find-history))))

  (let ((project-root (if (and (fboundp 'project-current)
                               (project-current))
                          (project-root (project-current))
                        default-directory)))
    (davidc-find-files pattern project-root)))

;;; Utility commands.

;;;###autoload
(defun davidc-find-check-backend ()
  "Display information about the currently configured backend.

Shows:
- Which backend is active (find or fd)
- Path to the executable
- Default arguments
- Current display format
- Version information"
  (interactive)
  (let* ((backend-name (if (eq davidc-find-backend 'fd) "fd" "find"))
         (executable (if (eq davidc-find-backend 'fd)
                         davidc-find-fd-executable
                       davidc-find-executable))
         (default-args (if (eq davidc-find-backend 'fd)
                           davidc-find-fd-default-args
                         davidc-find-find-default-args)))
    (if (executable-find executable)
        (let ((version-output (shell-command-to-string
                               (format "%s --version 2>/dev/null || echo 'Version information not available'"
                                       executable))))
          (message "Backend: %s\nExecutable: %s\nDefault args: %s\nDisplay format: %s\nVersion:\n%s"
                   backend-name
                   executable
                   (if default-args
                       (mapconcat #'identity default-args " ")
                     "None")
                   davidc-find-display-format
                   (string-trim version-output)))
      (error "Executable not found: %s. Please install %s or customize %s"
             executable
             backend-name
             (if (eq davidc-find-backend 'fd)
                 "`davidc-find-fd-executable'"
               "`davidc-find-executable'")))))

;;;###autoload
(defun davidc-find-set-backend (backend)
  "Set the find backend to BACKEND.

BACKEND can be \\='find or \\='fd.

Use \\='find for maximum compatibility and features.
Use \\='fd for better performance and simpler syntax."
  (interactive
   (list (intern (completing-read "Backend: "
                                  '("find" "fd")
                                  nil t nil nil
                                  (symbol-name davidc-find-backend)))))
  (setq davidc-find-backend backend)
  (message "Backend set to: %s" backend))

;;;###autoload
(defun davidc-find-set-display-format (format)
  "Set display format to FORMAT.

FORMAT can be:
- \\='abspath: Show absolute paths
- \\='ls: Show detailed listing (like ls -l)
- \\='relative: Show paths relative to search directory
- \\='basename: Show only filenames without paths"
  (interactive
   (list (intern (completing-read "Display format: "
                                  '("abspath" "ls" "relative" "basename")
                                  nil t nil nil
                                  (symbol-name davidc-find-display-format)))))
  (setq davidc-find-display-format format)
  (message "Display format set to: %s" format))

(provide 'davidc-find)

