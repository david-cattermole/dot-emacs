;;; -*- lexical-binding: t -*-
;;
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
  (unless hs-minor-mode
    (hs-minor-mode 1))
  (let (message-log-max
        (inhibit-message t))
    (if (= level 1)
        (pcase last-command
          ('davidc-hs-cycle
           (hs-hide-level 1)
           (setq this-command 'davidc-hs-cycle-children))
          ('davidc-hs-cycle-children
           ;; TODO: Fix this case. `hs-show-block' needs to be
           ;; called twice to open all folds of the parent
           ;; block.
           (save-excursion (hs-show-block))
           (hs-show-block)
           (setq this-command 'davidc-hs-cycle-subtree))
          ('davidc-hs-cycle-subtree
           (hs-hide-block))
          (_
           (if (not (hs-already-hidden-p))
               (hs-hide-block)
             (hs-hide-level 1)
             (setq this-command 'davidc-hs-cycle-children))))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level))))

;; Toggles the hide-show cycle for the entire buffer.
(defun davidc-hs-global-cycle ()
  (interactive)
  (unless hs-minor-mode
    (hs-minor-mode 1))
  (pcase last-command
    ('davidc-hs-global-cycle
     (save-excursion (hs-show-all))
     (setq this-command 'davidc-hs-global-show))
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

;; Expand when using a `goto-line` function (like it does in search
;; mode).
;;
;; https://www.emacswiki.org/emacs/HideShow#h5o-9
(defadvice goto-line (after expand-after-goto-line
                            activate compile)
  "hideshow-expand affected block when using goto-line in a collapsed buffer"
  (save-excursion
    (hs-show-block)))

(provide 'davidc-hideshow)
