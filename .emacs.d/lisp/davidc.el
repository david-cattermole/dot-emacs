;; A simple package for personal usage and learning.
;;
;;
;; http://xahlee.info/emacs/emacs/elisp_editing_basics.html
;; http://xahlee.info/emacs/emacs/elisp_examples.html


;; Open init.el file
(defun davidc-open-init-file ()
  ""
  (interactive)
  (find-file "~/.emacs.d/init.el"))


;; Open davidc.el file
(defun davidc-open-package-file ()
  ""
  (interactive)
  (find-file "~/.emacs.d/lisp/davidc.el"))


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


;; Fullscreen
(defun davidc-fullscreen (&optional f)
  (interactive)
  (set-frame-parameter f 'fullscreen
                       (if (frame-parameter f 'fullscreen) nil 'fullboth)))


;; Hide Dired details by default.
;;
;; http://xahlee.info/emacs/emacs/emacs_dired_tips.html
(defun davidc-dired-mode-details-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))
