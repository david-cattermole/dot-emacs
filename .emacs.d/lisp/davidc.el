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


(defun davidc-unfill-paragraph (&optional region)
  "Reverse the effect of `fill-paragraph' by joining all lines in the current paragraph into a single line.
If REGION is non-nil, operate on the active region instead."
  (interactive (list (use-region-p)))
  (when buffer-read-only
    (error "Cannot unfill paragraph. Buffer is read-only."))
  (let ((fill-column most-positive-fixnum)) ; Set fill-column to a very large value
    (if region
        (fill-region (region-beginning) (region-end))
      (fill-paragraph nil))))

(defun davidc-unfill-region (start end)
  "Unfill all paragraphs in the region from START to END."
  (interactive "r")  ; "r" means to use the region as arguments
  (when buffer-read-only
    (error "Cannot unfill region. Buffer is read-only."))
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

(defun davidc-unfill-buffer ()
  "Unfill all paragraphs in the current buffer, converting them into single lines."
  (interactive)
  (when buffer-read-only
    (error "Cannot unfill buffer. Buffer is read-only."))
  (let ((fill-column most-positive-fixnum))
    (fill-region (point-min) (point-max))))

;; Highlighting Lines
;;
;; This example shows you how to make lines containing the words
;; "ERROR:" or "NOTE:" highlighted, whenever a file ending in "log" is
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


(defun davidc-fullscreen (&optional f)
  "Toggle Fullscreen mode."
  (interactive)
  (if (version< emacs-version "25.0")
      (progn
        (set-frame-parameter f 'fullscreen
                             (if (frame-parameter f 'fullscreen) nil 'fullboth)))
    (progn
      ;; 'toggle-frame-fullscreen' was added in Emacs 24.4.
      (toggle-frame-fullscreen))))


;; Hide Dired details by default.
;;
;; http://xahlee.info/emacs/emacs/emacs_dired_tips.html
(defun davidc-dired-mode-details-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))


;; https://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
;;
;; This function assumes the user has a region active (ie. that both
;; mark-active and transient-mark-mode are non-nil).
(defun davidc-move-region-internal (line-offset-number)
  (when buffer-read-only
    (error "Cannot move region in read-only buffer."))

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
  (when buffer-read-only
    (error "Cannot move line in read-only buffer."))
  (transpose-lines 1)
  (forward-line -2))


(defun davidc-move-line-down ()
  "Move down the current line."
  (interactive)
  (when buffer-read-only
    (error "Cannot move line in read-only buffer."))
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


;; Find Perforce projects.
;;
;; Similar to the 'davidc-git-project-finder', but for Perforce (p4).
(defun davidc-perforce-project-finder (dir)
  "Integrate .p4rc project roots."
  (let ((dotgit (and (setq dir (locate-dominating-file dir ".p4rc"))
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
  (when buffer-read-only
    (error "Cannot modify text in read-only buffer."))
  (string-inflection-insert
   (davidc-string-inflection-toggle-function (string-inflection-get-current-word))))

(defun davidc-string-inflection-cycle-auto ()
  "Switching case cylcing by major-mode"
  (interactive)
  (when buffer-read-only
    (error "Cannot modify text in read-only buffer."))
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


(defun davidc-symbol-highlight-or-dired-details-toggle ()
  "Smart toggle function for symbol-highlight or dired-details.

In dired buffers: toggle dired-hide-details-mode.
In other buffers: toggle symbol highlight lock."
  (interactive)
  (cond
   ;; In dired mode, toggle details.
   ((derived-mode-p 'dired-mode)
    (if (fboundp 'dired-hide-details-mode)
        ;; Toggle `ls -1` (dash one) and `ls -l` (dash lower case L)
        ;; output in Dried.
        (dired-hide-details-mode 'toggle)
      (message "dired-hide-details-mode not available")))

   ;; In other buffers, toggle symbol highlight lock.
   (t
    (if (bound-and-true-p davidc-symbol-highlight-mode)
        (davidc-symbol-highlight-toggle-symbol-lock)
      ;; If symbol highlight mode is not active, enable it first.
      (progn
        (davidc-symbol-highlight-mode 1)
        (message "Enabled symbol highlighting mode"))))))


(provide 'davidc)
