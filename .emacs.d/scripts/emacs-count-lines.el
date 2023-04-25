#!/usr/bin/emacs --script
;;
;; This is an Emacs script to count the number of blank, source and
;; comment lines in all files given as arguments.
;;
;; Usage:
;; The script can be run with 'emacs --script emacs-count-lines.el',
;; or the file can be executed directly.
;;
;; The following pages were used for reference:
;; https://www.emacswiki.org/emacs/BatchMode
;; https://cslab.pepperdine.edu/warford/BatchIndentationEmacs.html
;; https://kitchingroup.cheme.cmu.edu/blog/2014/08/06/Writing-scripts-in-Emacs-lisp/
;;

(defun emacs-count-lines-main ()
  "Count the number of lines that are source code, and how many are comments."

  ;; (message (version))
  ;; (message (format "Emacs called with arguments: %s" command-line-args))
  ;; (message (format "Command line args: %s" command-line-args-left))

  (let (total-source-count total-blank-count total-comment-count)
    (dolist (element command-line-args-left total-source-count)
      (find-file element)

      (setq file-name (file-name-nondirectory (buffer-file-name)))
      ;; (message (format "Opening file name: %s" file-name))

      (let (line-count-source line-count-comment line-count-blank)
        (setq line-count-source 0)
        (setq line-count-comment 0)
        (setq line-count-blank 0)

        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring (point) (progn (end-of-line) (point))))
                (line-number (line-number-at-pos))
                (is-blank-line (looking-at-p "^[[:blank:]]*$"))
                (is-comment-line
                 (cond
                  ((nth 4 (syntax-ppss)) t)
                  (t nil))))

            (setq line-type
                  (cond
                   (is-blank-line   (progn "BLK"))
                   (is-comment-line (progn "COM"))
                   (t               (progn "SRC"))
                   ))
            ;; ;; Print the contents of the file, and the line type.
            ;; (message (format "%s Line %s: %s" line-type line-number line))

            (cond
             (is-blank-line (setq line-count-blank (+ (or line-count-blank 0) 1)))
             (is-comment-line (setq line-count-comment (+ (or line-count-comment 0) 1)))
             ((not is-comment-line) (setq line-count-source (+ (or line-count-source 0) 1)))
             )

            ;; Iterate to the next line.
            (forward-line 1)
            )
        )

        (message (format "SOURCE: %s COMMENTS: %s BLANK: %s\t%s"
                         line-count-source
                         line-count-comment
                         line-count-blank
                         file-name))

        (setq total-source-count (cons line-count-source total-source-count))
        (setq total-blank-count (cons line-count-blank total-blank-count))
        (setq total-comment-count (cons line-count-comment total-comment-count))
        )
    )

    (message (format "Number of source lines:  %s" (apply '+ total-source-count)))
    (message (format "Number of blank lines:   %s" (apply '+ total-blank-count)))
    (message (format "Number of comment lines: %s" (apply '+ total-comment-count)))
  ))

;; We can call the main function only when this file is called as a
;; script (with 'emacs --script filename.el').
(when (member "-scriptload" command-line-args)
  (emacs-count-lines-main))

(provide 'command)
;;; command.el ends here
