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
           (save-excursion
             (hs-show-block)
             (hs-show-block))
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
                                  (? (or "unsafe" "async" "const" "extern") (+ space))
                                  ;; TODO: Do we need to handle angle
                                  ;; braces "<>", or any other syntax?
                                  (or "impl" "trait" "mod")
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

(defun davidc-css-forward-block (_arg)
  "Move forward over a CSS block by finding the matching closing brace."
  (let ((depth 1))
    (forward-char 1)
    (while (and (> depth 0) (re-search-forward "[{}]" nil t))
      (unless (nth 8 (syntax-ppss))
        (if (string= (match-string 0) "{")
            (setq depth (1+ depth))
          (setq depth (1- depth)))))
    (= depth 0)))

(defun davidc-css-hideshow-setup ()
  "Set up custom hideshow behaviour for CSS mode.
Uses buffer-local variables instead of `hs-special-modes-alist'
to ensure hideshow works regardless of syntax table support."
  (when (derived-mode-p 'css-mode 'css-ts-mode)
    (setq-local hs-block-start-regexp "{")
    (setq-local hs-block-end-regexp "}")
    (setq-local hs-forward-sexp-func #'davidc-css-forward-block)
    (setq-local hs-hide-all-non-comment-function nil)))

(defun davidc-js-hideshow-setup ()
  "Set up custom hideshow behaviour for JavaScript mode."
  (add-to-list 'hs-special-modes-alist
               '(js-mode
                 "{" "}" "//" forward-sexp
                 hs-c-like-adjust-block-beginning
                 nil nil nil)))

(defun davidc-mhtml-hs-forward (arg)
  "Move forward over an HTML/embedded block for hideshow.
When point is in an embedded CSS/JS submode, use `forward-sexp'.
Otherwise, skip forward past the HTML tag element using
`sgml-skip-tag-forward'."
  (interactive "P")
  (pcase (get-text-property (point) 'mhtml-submode)
    (`nil (sgml-skip-tag-forward 1))
    (submode (forward-sexp 1))))

(defun davidc-html-hideshow-setup ()
  "Set up custom hideshow behaviour for HTML mode."
  (add-to-list 'hs-special-modes-alist
               '(mhtml-mode
                   "{\\|<[^/>]+?"
                 "}\\|</[^/>]*[^/]>"
                 "<!--"
                 davidc-mhtml-hs-forward
                 nil nil nil nil)))

(defun davidc-json-hs-hide-all-nested ()
  "Hide nested JSON blocks, keeping the top level visible.
Only hides blocks that are not at the top level of the JSON structure."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "{" nil t)
      (backward-char)
      (when (and (funcall hs-looking-at-block-start-p-func)
                 (> (car (syntax-ppss)) 0))
        (hs-hide-block-at-point t))
      (forward-char 1))))

(defun davidc-json-hideshow-setup ()
  "Set up custom hideshow behaviour for JSON mode."
  (add-to-list 'hs-special-modes-alist
               '(js-json-mode
                 "{" "}" nil forward-sexp
                 nil nil nil nil))
  (when (derived-mode-p 'js-json-mode)
    (setq-local hs-hide-all-non-comment-function
                #'davidc-json-hs-hide-all-nested)))

(provide 'davidc-hideshow)
