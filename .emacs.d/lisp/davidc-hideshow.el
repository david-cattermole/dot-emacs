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

(defun davidc-python-hs-hide-all-non-class ()
  "Hide all Python blocks except class definitions.
This function is meant to be used as `hs-hide-all-non-comment-function'
for Python mode."
  (save-excursion
    (goto-char (point-min))
    (while (python-nav-forward-block)
      (when (funcall hs-looking-at-block-start-p-func)
        ;; Check if this is NOT a class definition
        (unless (looking-at (rx line-start (* space) "class" (+ space)))
          (hs-hide-block-at-point t))
        ;; Move past the current block to continue searching
        (goto-char (match-end 0))))))

(defun davidc-python-hideshow-setup ()
  "Set up custom hideshow behaviour for Python mode.
Classes will not be hidden when running `hs-hide-all'."
  (when (derived-mode-p 'python-mode 'python-ts-mode)
    (setq-local hs-hide-all-non-comment-function
                #'davidc-python-hs-hide-all-non-class)))

(defun davidc-rust-hs-hide-all-non-module ()
  "Hide all Rust blocks except module definitions.
This function is meant to be used as `hs-hide-all-non-comment-function'
for Rust mode."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "{" nil t)
      (goto-char (match-beginning 0))
      (when (funcall hs-looking-at-block-start-p-func)
        ;; Check if this is NOT a module definition
        (unless (save-excursion
                  (beginning-of-line)
                  (looking-at (rx line-start
                                  (* space)
                                  (? (or "pub" "pub(crate)" "pub(super)") (+ space))
                                  (? (or "unsafe" "async") (+ space))
                                  "mod"
                                  (+ space))))
          (hs-hide-block-at-point t)))
      ;; Move past this { to continue searching
      (forward-char 1))))

(defun davidc-rust-hideshow-setup ()
  "Set up custom hideshow behaviour for Rust mode.
Modules will not be hidden when running `hs-hide-all'."

  ;; Add code folding regular expressions for Rust
  ;;
  ;; Based on the Ruby implementation here;
  ;; https://gist.github.com/Karina7777/e6207b027af0b391ff38
  (when davidc-config-use-rust-mode
    (add-to-list 'hs-special-modes-alist
                 '(rust-mode
                   "{" ;; Block start.
                   "}" ;; Block end.
                   ;; NOTE: Does not handle comments with "/* */" style.
                   "//" ;; Comment start.
                   forward-sexp ;; FORWARD-SEXP-FUNC
                   hs-c-like-adjust-block-beginning ;; ADJUST-BEG-FUNC
                   nil  ;; FIND-BLOCK-BEGINNING-FUNC
                   nil  ;; FIND-NEXT-BLOCK-FUNC
                   nil  ;; LOOKING-AT-BLOCK-START-P-FUNC
                   ))
    )

  (when (derived-mode-p 'rust-mode 'rust-ts-mode)
    (setq-local hs-hide-all-non-comment-function
                #'davidc-rust-hs-hide-all-non-module)))

(defun davidc-cpp-hs-hide-all-non-namespace ()
  "Hide all C++ blocks except namespace definitions.
This function is meant to be used as `hs-hide-all-non-comment-function'
for C++ mode."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "{" nil t)
      (goto-char (match-beginning 0))
      (when (funcall hs-looking-at-block-start-p-func)
        ;; Check if this is NOT a namespace definition.
        (unless (save-excursion
                  (beginning-of-line)
                  (looking-at (rx line-start
                                  (* space)
                                  "namespace"
                                  (or (+ space) line-end))))
          (hs-hide-block-at-point t)))
      ;; Move past this { to continue searching.
      (forward-char 1))))

(defun davidc-cpp-hideshow-setup ()
  "Set up custom hideshow behaviour for C++ mode.
Namespaces will not be hidden when running `hs-hide-all'."
  (when (derived-mode-p 'c++-mode 'c++-ts-mode 'c-mode)
    (setq-local hs-hide-all-non-comment-function
                #'davidc-cpp-hs-hide-all-non-namespace)))

(provide 'davidc-hideshow)
