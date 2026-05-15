;;; -*- lexical-binding: t -*-
;;
;; Enhanced HTML/SGML tag navigation.
;;
;; This package provides improved navigation keybindings for HTML and
;; SGML modes, complementing the existing `sgml-skip-tag-forward' and
;; `sgml-skip-tag-backward' functions with directional arrow keys and
;; adding parent/child (up/down) tag hierarchy navigation.
;;
;; Keybindings (set up by `davidc-sgml-setup-keybindings'):
;;   C-c C-<right>  - `davidc-sgml-skip-tag-forward'
;;   C-c C-<left>   - `davidc-sgml-skip-tag-backward'
;;   C-c C-<up>     - `davidc-sgml-up-element'
;;   C-c C-<down>   - `davidc-sgml-down-element'

(require 'sgml-mode)

(defun davidc-sgml-skip-tag-forward ()
  "Move forward across one tag element.
Wrapper around `sgml-skip-tag-forward' for keybinding convenience."
  (interactive)
  (sgml-skip-tag-forward 1))

(defun davidc-sgml-skip-tag-backward ()
  "Move backward across one tag element.
Wrapper around `sgml-skip-tag-backward' for keybinding convenience."
  (interactive)
  (sgml-skip-tag-backward 1))

(defun davidc-sgml--get-context-safe ()
  "Return `sgml-get-context', handling point on `<` of an opening tag.
When point is exactly on the `<` character that starts an opening
tag, `sgml-get-context' excludes that tag from the context.  This
function temporarily moves point into the tag name to get the
correct context, then restores the original position."
  (let ((start (point)))
    (when (and (= (char-after start) ?<)
               (not (looking-at "</")))
      (forward-char 1))
    (let ((ctx (sgml-get-context)))
      (goto-char start)
      ctx)))

(defun davidc-sgml-up-element ()
  "Move point up to the parent HTML/SGML element.
Jumps to the opening `<` of the enclosing parent tag."
  (interactive)
  (let* ((ctx (davidc-sgml--get-context-safe))
         (tag (car (last ctx)))
         parent)
    (if (null tag)
        (message "No enclosing tag found")
      ;; Go to the start of the innermost tag.
      (goto-char (aref tag 2))
      ;; Look backward for its parent tag.
      (setq parent (condition-case nil
                       (sgml-parse-tag-backward)
                     (error nil)))
      (if (null parent)
          (message "No parent tag found")
        (goto-char (aref parent 2))))))

(defun davidc-sgml-down-element ()
  "Move point down into the first child tag.
Jumps to the opening `<` of the first tag nested inside the
current element."
  (interactive)
  (let* ((ctx (davidc-sgml--get-context-safe))
         (tag (car (last ctx))))
    (if (null tag)
        (message "No enclosing tag found")
      ;; Move to the end of the opening tag (after `>').
      (goto-char (aref tag 3))
      ;; Search forward for the next opening tag.
      (if (re-search-forward "<[[:alnum:]]" nil t)
          (goto-char (match-beginning 0))
        (message "No child tag found")))))

(defun davidc-sgml-setup-keybindings ()
  "Set up SGML/HTML navigation keybindings in the current buffer.
Should be added to `sgml-mode-hook', `html-mode-hook', and
`mhtml-mode-hook'."
  (local-set-key (kbd "C-c C-<right>") 'davidc-sgml-skip-tag-forward)
  (local-set-key (kbd "C-c C-<left>") 'davidc-sgml-skip-tag-backward)
  (local-set-key (kbd "C-c C-<up>") 'davidc-sgml-up-element)
  (local-set-key (kbd "C-c C-<down>") 'davidc-sgml-down-element))

(provide 'davidc-sgml)
