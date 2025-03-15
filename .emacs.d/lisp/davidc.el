;;; -*- lexical-binding: t -*-
;; A simple package for personal usage and learning.
;;
;;
;; http://xahlee.info/emacs/emacs/elisp_editing_basics.html
;; http://xahlee.info/emacs/emacs/elisp_examples.html


(defun davidc-open-init-file ()
  "Open init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))


(defun davidc-open-package-file ()
  "Open davidc.el file."
  (interactive)
  (find-file "~/.emacs.d/lisp/davidc.el"))


(defun davidc-open-custom-vars ()
  "Open custom-vars.el file."
  (interactive)
  ;; *custom-vars-file* is a global variable.
  (find-file *custom-vars-file*))


;; ;; Insert Text
;; ;;
;; ;; This code shows how to insert a string, and also position cursor
;; ;; after the insertion.
;; (defun davidc-insert-p-tag ()
;;   "Insert <p></p> at cursor point."
;;   (interactive)
;;   (insert "<p></p>")
;;   (backward-char 4))


;; ;; Insert Around Region
;; ;;
;; ;; This code shows how to place a string at the beginning and end of a
;; ;; region.
;; (defun davidc-wrap-markup-region ()
;;   "Insert a markup <b></b> around a region."
;;   (interactive)
;;   (let ((p1 (region-beginning))
;;         (p2 (region-end)))
;;     (goto-char p2)
;;     (insert "</b>")
;;     (goto-char p1)
;;     (insert "<b>")))

;; ;; Select Current Word
;; ;;
;; ;; This code shows you how to set a mark (select text) programmatically.
;; (defun davidc-select-current-word ()
;;   "Select the word under cursor.
;; “word” here is considered any alphanumeric sequence with “_” or “-”."
;;   (interactive)
;;   (let (pt)
;;     (skip-chars-backward "-_A-Za-z0-9")
;;     (setq pt (point))
;;     (skip-chars-forward "-_A-Za-z0-9")
;;     (set-mark pt)))


;; ;; Select Current Line
;; (defun davidc-select-current-line ()
;;   "Select the current line"
;;   (interactive)
;;   (let ((pos (line-beginning-position)))
;;     (end-of-line)
;;     (set-mark pos)))


;; ;; Find Replace String in Region
;; ;;
;; ;; Here's how to do text replacements on a region.
;; ;;
;; (defun davidc-replace-greek-region ()
;;   "Replace “alpha” to “α” and other greek letters in current
;; region."
;;   (interactive)
;;   (let (
;;         (p1 (region-beginning))
;;         (p2 (region-end)))
;;     (save-restriction
;;       (narrow-to-region p1 p2)
;;       (goto-char (point-min))
;;       (while (search-forward " alpha" nil t)
;;         (replace-match " α" nil t))
;;       (goto-char (point-min))
;;       (while (search-forward " beta" nil t)
;;         (replace-match " β" nil t))
;;       (goto-char (point-min))
;;       (while (search-forward " gamma" nil t)
;;         (replace-match " γ" nil t)))))


;; ;; Delete Enclosed Text
;; ;;
;; ;; This code shows how to delete text enclosed by any pairs of delimiters.
;; (defun davidc-delete-enclosed-text ()
;;   "Delete texts between any pair of delimiters."
;;   (interactive)
;;   (save-excursion
;;     (let (p1 p2)
;;       (skip-chars-backward "^([<>“")
;;       (setq p1 (point))
;;       (skip-chars-forward "^)]<>”")
;;       (setq p2 (point))
;;       (delete-region p1 p2))))


;; ;; Delete Linebreaks
;; ;;
;; ;; This example shows how to temporarily change a predefined
;; ;; variable's value, then call a function whose behavior dep ends on
;; ;; the var.
;; (defun davidc-remove-line-breaks ()
;;   "Remove line endings in current paragraph."
;;   (interactive)
;;   (let ((fill-column (point-max)))
;;     (fill-paragraph nil)))


;; ;; Inserting a Random Number
;; (random t) ; seed it randomly
;; (defun davidc-insert-random-number ()
;;   "Insert a random number between 0 to 999999."
;;   (interactive)
;;   (insert (number-to-string (random 999999))) )


;; ;; print current word.
;; (defun ff ()
;;   "print current word."
;;   (interactive)
;;   (message "%s" (thing-at-point 'word)))


;; ;; Get Boundary Positions of a Thing
;; ;;
;; ;; Sometimes you also need to know a thing's boundary, because you may
;; ;; need to delete it (using (delete-region position1 position2)).
;; ;;
;; ;; bounds-of-thing-at-point
;; ;;
;; ;; Return a cons cell (cons pos1 pos2) that's the boundary positions
;; ;; of the text unit under cursor.
;; (defun davidc-my-get-boundary-and-thing ()
;;   "example of using `bounds-of-thing-at-point'"
;;   (interactive)
;;   (let (bounds pos1 pos2 mything)
;;     (setq bounds (bounds-of-thing-at-point 'symbol))
;;     (setq pos1 (car bounds))
;;     (setq pos2 (cdr bounds))
;;     (setq mything (buffer-substring-no-properties pos1 pos2))
;;     (message
;;      "thing begin at [%s], end at [%s], thing is [%s]"
;;      pos1 pos2 mything)))


;; (defun append-to-buffer (buffer start end)
;;   "Append the text of the region to BUFFER."
;;   (interactive "BAppend to buffer: \nr")
;;   (let ((oldbuf (current-buffer)))
;;     (save-current-buffer
;;       (set-buffer (get-buffer-create buffer))
;;       (insert-buffer-substring oldbuf start end))))


;; ;; Part of gnu emacs
;; (require 'browse-url)


;; ;; Reference Lookup
;; ;;
;; ;; This example shows the use of thing-at-point and browse-url.
;; ;;
;; ;; It will look up the word under the cursor in a online dictionary.
;; (defun davidc-lookup-word ()
;;   "Look up the word under cursor in a browser."
;;   (interactive)
;;   (browse-url
;;    (concat "https://duckduckgo.com/?q=" (thing-at-point 'symbol))))


;; ;; Look up selected region in search engine.
;; (defun davidc-lookup-region ()
;;   "Look up the word under cursor in a search engine.
;; If there is a text selection (a phrase), use that.  This command
;; switches to browser."
;;   (interactive)
;;   (let (word)
;;     (setq word
;;           (if (use-region-p)
;;               (buffer-substring-no-properties (region-beginning) (region-end))
;;             (current-word)))
;;     (browse-url
;;      (concat
;;       "https://duckduckgo.com/?q="
;;       (string-replace "(" "" (string-replace ")" "" (format "%s" (split-string (string-replace "-mode" "" (format "%s" major-mode)) "-"))))
;;       " "
;;       word)
;;      )
;;     )
;;   )


;; ;; Delete Current File
;; ;;
;; ;; This example shows command that lets you delete the current
;; ;; file. Note here that elisp is used to: {manipulate buffer,
;; ;; manipulate file, prompt user}.
;; (defun davidc-delete-current-file ()
;;   "Delete the file associated with the current buffer.
;; Delete the current buffer too.
;; If no file is associated, just close buffer without prompt for save."
;;   (interactive)
;;   (let ((currentFile (buffer-file-name)))
;;     (when (yes-or-no-p (concat "Delete file?: " currentFile))
;;       (kill-buffer (current-buffer))
;;       (when currentFile
;;         (delete-file currentFile)))))


;; Highlighting Lines
;;
;; This example shows you how to make lines containing the words
;; “ERROR:” or “NOTE:” highlighted, whenever a file ending in “log” is
;; opened.
(defun davidc-highlight-it ()
  "Highlight certain lines."
  (interactive)
  (if (equal "log" (file-name-extension (buffer-file-name)))
    (progn
      (highlight-lines-matching-regexp "ERROR:" 'hi-red-b)
      (highlight-lines-matching-regexp "WARN:" 'hi-yellow-b)
      (highlight-lines-matching-regexp "WARNING:" 'hi-yellow-b)
      (highlight-lines-matching-regexp "NOTE:" 'hi-green-b)
      )
    (progn
      (highlight-phrase "ERROR:" 'hi-red-b)
      (highlight-phrase "TODO:" 'hi-green-b)
      (highlight-phrase ".. todo::" 'hi-green-b)
      (highlight-phrase "NOTE:" 'hi-green-b)
      (highlight-phrase ".. note::" 'hi-green-b)
      )
    ))

;; (defun davidc-highlight-it ()
;;   "Highlight certain lines."
;;   (interactive)
;;   (if (equal "log" (file-name-extension (buffer-file-name)))
;;       (progn
;;         (highlight-lines-matching-regexp "ERROR:" 'hi-red-b)
;;         (highlight-lines-matching-regexp "NOTE:" 'hi-blue-b))))


;; ;; Emacs Lisp: Write Emacs Command Using Python etc
;; ;;
;; ;; Write a script in your favorite language and make it into a emacs
;; ;; command!
;; ;;
;; ;; Here's the elisp wrapper:
;; (defun davidc-do-something-region (startPos endPos)
;;   "Do some text processing on region.
;; This command calls the external script “wc”."
;;   (interactive "r")
;;   (let (cmdStr)
;;     (setq cmdStr "/usr/bin/wc") ; full path to your script
;;     (shell-command-on-region startPos endPos cmdStr nil t nil t)))


;; ;; Passing Text to STDIN and Command Line Arg
;; ;;
;; ;; In this example, we also pass a argument to the shell command.
;; ;;
;; ;; You can simply do that by passing it as part of the command string.
;; (defun davidc-my-call-script-xyz ()
;;   "example of calling a external command. passing text of region to
;; its stdin.  and passing current file name to the script as arg.
;; replace region by its stdout."
;;   (interactive)
;;   (let ((cmdStr
;;          (format
;;           "/usr/bin/python /home/joe/pythonscriptxyz %s"
;;           (buffer-file-name))))
;;     (shell-command-on-region (region-beginning) (region-end) cmdStr nil "REPLACE" nil t)))


(defun davidc-fullscreen (&optional f)
  "Toggle Fullscreen mode."
  (interactive)
  (set-frame-parameter f 'fullscreen
                       (if (frame-parameter f 'fullscreen) nil 'fullboth)))


;; Hide Dired details by default.
;;
;; http://xahlee.info/emacs/emacs/emacs_dired_tips.html
(defun davidc-dired-mode-details-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))


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


(defun davidc-format-region-python ()
  "Formats a region of Python code.
For now we use 'python black'."
  (interactive)
  (call-interactively 'python-black-region))


(defun davidc-format-buffer-python ()
  "Formats a Python buffer.
For now we use 'python black'."
  (interactive)
  (call-interactively 'python-black-buffer))


(defun davidc-format ()
  "Format the buffer or region for supported major modes.
Supported major modes are C++ (c++-mode) and Python (python-mode)."
  (interactive)
  ;; http://xahlee.info/emacs/emacs/emacs_region.html
  (if (use-region-p)
      (call-interactively 'davidc-format-region)
    (call-interactively 'davidc-format-buffer)))


(defun davidc-format-region ()
  "Format the selected region of text for supported major modes.
Supported major modes are C++ (c++-mode) and Python (python-mode)."
  (interactive)
  (cond
   ((string-equal major-mode "c++-mode") (call-interactively 'davidc-format-region-c++))
   ((string-equal major-mode "python-mode") (call-interactively 'davidc-format-region-python))
   )
  )


(defun davidc-format-buffer ()
  "Format the selected buffer of text for supported major modes.
Supported major modes are C++ (c++-mode) and Python (python-mode)."
  (interactive)
  (cond
   ((string-equal major-mode "c++-mode") (call-interactively 'davidc-format-buffer-c++))
   ((string-equal major-mode "python-mode") (call-interactively 'davidc-format-buffer-python))
   )
  )

;; https://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
;;
;; This function assumes the user has a region active (ie. that both
;; mark-active and transient-mark-mode are non-nil).
(defun davidc-move-region-internal (line-offset-number)
   ;; Makes the point (cursor location) go to the top of the active
   ;; (selection) region.
   (if (> (point) (mark))
       (exchange-point-and-mark))

   ;; Ensure the region uses contents of the first line, so the line
   ;; is not chopped off.
   (move-to-column 0 t)

   ;; Move active (selection) region up/down
   (let ((text (delete-and-extract-region (point) (mark))))
     (forward-line line-offset-number)
     (set-mark (point))
     (insert text)
     (exchange-point-and-mark)
     (setq deactivate-mark nil))
   )


(defun davidc-move-region-down (line-offset-number)
  "Move region (transient-mark-mode active)
  line-offset-number lines down."
  (interactive "*p")
  (davidc-move-region-internal line-offset-number))


(defun davidc-move-region-up (line-offset-number)
  "Move region (transient-mark-mode active)
  line-offset-number lines up."
  (interactive "*p")
  (davidc-move-region-internal (- line-offset-number)))


;; https://emacs.stackexchange.com/questions/13941/move-selected-lines-up-and-down
(defun davidc-move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))


(defun davidc-move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))


(defun davidc-move-line-or-region-up ()
  "Move up the active region, or the current line."
  (interactive)
  (if (and mark-active transient-mark-mode)
      (call-interactively 'davidc-move-region-up)
    (call-interactively 'davidc-move-line-up)))


(defun davidc-move-line-or-region-down ()
  "Move down the active region, or the current line."
  (interactive)
  (if (and mark-active transient-mark-mode)
      (call-interactively 'davidc-move-region-down)
    (call-interactively 'davidc-move-line-down)))


;; As the built-in project.el support expects to use vc-mode hooks to
;; find the root of projects we need to provide something equivalent
;; for it.
;;
;; https://www.reddit.com/r/emacs/comments/nf2k5y/comment/gyjs516/?utm_source=share&utm_medium=web2x&context=3
(defun davidc-git-project-finder (dir)
  "Integrate .git project roots."
  (let ((dotgit (and (setq dir (locate-dominating-file dir ".git"))
                     (expand-file-name dir))))
    (and dotgit
         (cons 'transient (file-name-directory dotgit)))))


(defun davidc-string-inflection-toggle-function (str)
  "Not so much the case that in all caps when using normal foo_bar <--> fooBar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-lower-camelcase-function str))
   ((string-inflection-lower-camelcase-p str)
    (string-inflection-underscore-function str))
   (t
    (string-inflection-underscore-function str))))

(defun davidc-string-inflection-toggle ()
  "Toggle foo_bar <=> fooBar"
  (interactive)
  (string-inflection-insert
   (davidc-string-inflection-toggle-function (string-inflection-get-current-word))))

(defun davidc-string-inflection-cycle-auto ()
  "Switching case cylcing by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))

   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))

   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))

   ;; for elixir
   ((eq major-mode 'elixir-mode)
    (string-inflection-elixir-style-cycle))

   ;; default
   (t
    (string-inflection-python-style-cycle)))
  )


;; Code folding cycling.
;;
;; https://karthinks.com/software/simple-folding-with-hideshow/
;;
;; Note: This code looks like it has some redundant clauses you can
;; refactor using hs-already-hidden-p, and like you don't need to set
;; last-command for all the clauses. Don’t try this, it breaks in
;; subtle ways.
(defun davidc-hs-cycle (&optional level)
  (interactive "p")
  (let (message-log-max
        (inhibit-message t))
    (if (= level 1)
        (pcase last-command
          ('davidc-hs-cycle
           (hs-hide-level 1)
           (setq this-command 'hs-cycle-children))
          ('hs-cycle-children
           ;; TODO: Fix this case. `hs-show-block' needs to be
           ;; called twice to open all folds of the parent
           ;; block.
           (save-excursion (hs-show-block))
           (hs-show-block)
           (setq this-command 'hs-cycle-subtree))
          ('hs-cycle-subtree
           (hs-hide-block))
          (_
           (if (not (hs-already-hidden-p))
               (hs-hide-block)
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level))))

;; Toggles the hide-show cycle for the entire buffer.
(defun davidc-hs-global-cycle ()
  (interactive)
  (pcase last-command
    ('davidc-hs-global-cycle
     (save-excursion (hs-show-all))
     (setq this-command 'hs-global-show))
    (_ (hs-hide-all))))


;; Display how many line numbers a folded area shows.
;;
;; From the documentation, but modified a little bit.
(defun davidc-hs-display-code-line-counts (ov)
   (when (eq 'code (overlay-get ov 'hs))
     (overlay-put ov 'display
                  (format "... [%d lines]"
                          (count-lines (overlay-start ov)
                                       (overlay-end ov))))))

;; Try the following to display the overlay content in a tooltip.
;;
;; https://www.emacswiki.org/emacs/HideShow#h5o-7
(defun davidc-hs-display-code-as-tooltip (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'help-echo
                 (buffer-substring (overlay-start ov)
                                   (overlay-end ov)))))

;; This is intended for debugging the emacs configuration. It can be
;; run to restart Emacs in a fresh state.
(defun davidc-debug-restart-emacs ()
  "Restart Emacs completely for debugging purposes."
  (interactive)
  (let ((init-file user-init-file)
        (restart-args command-line-args))

    ;; Start a new Emacs process
    (call-process (concat invocation-directory invocation-name)
                  nil 0 nil
                  "--eval" "(message \"Restarted Emacs for debugging.\")"
                  "--file" init-file)

    ;; Exit this Emacs process
    (kill-emacs)))

;; This is intended for debugging the emacs configuration. It can be
;; run to restart Emacs in a fresh state.
(defun davidc-debug-restart-emacs-preserve-buffers ()
  "Restart Emacs completely for debugging purposes while preserving open buffers."
  (interactive)
  (let ((init-file user-init-file)
        (buf-names (mapcar 'buffer-file-name
                           (seq-filter 'buffer-file-name (buffer-list))))
        (current-buf (buffer-file-name))
        (desktop-file (expand-file-name "emacs-restart-buffers.el" temporary-file-directory)))

    ;; Save buffer list to temporary file
    (with-temp-file desktop-file
      (insert "(progn\n")
      ;; Reopen all files.
      (dolist (file buf-names)
        (when file
          (insert (format "  (find-file \"%s\")\n" file))))
      ;; Return to current buffer.
      (when current-buf
        (insert (format "  (find-file \"%s\")\n" current-buf)))
      (insert "  (message \"Emacs restarted with %d buffers restored.\"))\n"
              (length buf-names)))

    ;; Start a new Emacs process.
    (call-process (concat invocation-directory invocation-name)
                  nil 0 nil
                  "--eval" (format "(load \"%s\")" desktop-file)
                  "--file" init-file)

    ;; Exit this Emacs process.
    (kill-emacs)))

;; Define default settings if not already defined in custom-vars.el
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


;; Add Clang-tidy as a backend for Flymake.
;;
;; This uses this annotated example for ruby as a starting point:
;; https://www.gnu.org/software/emacs/manual/html_node/flymake/An-annotated-example-backend.html
(defvar-local davidc--flymake-clang-tidy-proc nil
  "Current clang-tidy flymake process.")

(defun davidc-flymake-clang-tidy (report-fn &rest _args)
  "Flymake backend for clang-tidy.
REPORT-FN is the callback function for reporting diagnostics."
  ;; Check if clang-tidy exists.
  (unless (executable-find "clang-tidy")
    (error "Cannot find clang-tidy executable"))

  ;; Kill any existing process.
  (when (process-live-p davidc--flymake-clang-tidy-proc)
    (kill-process davidc--flymake-clang-tidy-proc))

  ;; Save the current buffer.
  (let* ((source (current-buffer))
         (source-file (buffer-file-name)))

    (when source-file
      (save-restriction
        (widen)
        ;; Create a temporary file for the current buffer contents.
        (let ((temp-file (make-temp-file "flymake-clang-tidy" nil
                                         (file-name-extension source-file t))))
          ;; Write buffer contents to the temp file.
          (write-region nil nil temp-file nil 'quiet)

          ;; Reset the process variable and create a new process with
          ;; improved options.
          (setq davidc--flymake-clang-tidy-proc
                (make-process
                 :name "flymake-clang-tidy"
                 :noquery t
                 :connection-type 'pipe
                 :buffer (generate-new-buffer " *flymake-clang-tidy*")
                 :command (list "clang-tidy"
                                "-quiet"
                                temp-file
                                "-checks=-*,modernize-*,readability-*,bugprone-*,clang-analyzer-*,misc-*,performance-*,cppcoreguidelines-*"
                                "--")
                 :sentinel
                 (lambda (proc _event)
                   ;; Check that the process has indeed exited.
                   (when (memq (process-status proc) '(exit signal))
                     (unwind-protect
                         ;; Only proceed if proc is the current process.
                         (if (with-current-buffer source (eq proc davidc--flymake-clang-tidy-proc))
                             (with-current-buffer (process-buffer proc)
                               (goto-char (point-min))
                               (let ((diags '()))
                                 ;; This will match both the temp file
                                 ;; and original file paths.
                                 (while (search-forward-regexp
                                         "\\(?:.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(warning\\|error\\|note\\): \\(.*?\\)\\(?: \\[.*\\]\\)?$"
                                         nil t)
                                   (let* ((line (string-to-number (match-string 1)))
                                          (col (string-to-number (match-string 2)))
                                          (type (match-string 3))
                                          (msg (match-string 4))
                                          (level (cond
                                                  ((string= type "error") :error)
                                                  ((string= type "warning") :warning)
                                                  (t :note)))
                                          ;; Get region for diagnostics.
                                          (beg-end (flymake-diag-region
                                                   source
                                                   line
                                                   col)))
                                     ;; Check that we have valid positions.
                                     (when beg-end
                                       (push (flymake-make-diagnostic source
                                                                    (car beg-end)
                                                                    (cdr beg-end)
                                                                    level
                                                                    msg)
                                             diags))))
                                 ;; Report the diagnostics.
                                 (message "Found %d diagnostics" (length diags))
                                 (funcall report-fn diags)))
                           ;; If obsolete, log warning.
                           (flymake-log :warning "Canceling obsolete check %s" proc))

                       ;; Clean up resources.
                       (when (process-buffer proc)
                         (kill-buffer (process-buffer proc)))
                       (when (file-exists-p temp-file)
                         (delete-file temp-file))))))))))))

(defun davidc-flymake-clang-tidy-setup ()
  "Set up flymake for clang-tidy."
  (interactive)
  (message "Setting up clang-tidy for flymake.")
  ;; Add our clang-tidy backend to the list of diagnostic functions.
  (add-hook 'flymake-diagnostic-functions #'davidc-flymake-clang-tidy nil t)
  ;; Enable flymake.
  (flymake-mode 1))
