;;; davidc-ido.el --- Ido custom functions -*- lexical-binding: t -*-

(require 'ido)

(defun ido-disable-line-truncation ()
  "Disable line truncation in the current buffer."
  (set (make-local-variable 'truncate-lines) nil))

(defun ido-define-keys ()
  "Custom keybindings for Ido completion map."
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

(defun ido-goto-symbol ()
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (let ((ido-mode ido-mode)
        (ido-enable-flex-matching
         (if (boundp 'ido-enable-flex-matching)
             ido-enable-flex-matching t))
        (name-and-pos nil)
        (symbol-names nil)
        (selected-symbol nil))
    (unless ido-mode
      (ido-mode 1)
      (setq ido-enable-flex-matching t))

    (letrec ((collect-symbols
              (lambda (symbol-list)
                (dolist (symbol symbol-list)
                  (let (name position)
                    (cond
                     ((and (listp symbol) (imenu--subalist-p symbol))
                      (funcall collect-symbols (cdr symbol)))
                     ((listp symbol)
                      (setq name (car symbol))
                      (setq position (cdr symbol)))
                     ((stringp symbol)
                      (setq name symbol)
                      (setq position
                            (get-text-property 1 'org-imenu-marker symbol))))
                    (unless (or (null position) (null name)
                                (string= (car imenu--rescan-item) name))
                      (push name symbol-names)
                      (push (cons name position) name-and-pos)))))))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (funcall collect-symbols (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " (reverse symbol-names)))
               (string= (car imenu--rescan-item) selected-symbol))))

    (unless (and (boundp 'mark-active) mark-active)
      (push-mark nil t nil))
    (let ((position (cdr (assoc selected-symbol name-and-pos))))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position))))))

(defun ibuffer-ido-find-file (file &optional wildcards)
  "Like `ido-find-file', but default to the directory of the buffer at point."
  (interactive
   (let ((default-directory
           (let ((buf (ibuffer-current-buffer)))
             (if (buffer-live-p buf)
                 (with-current-buffer buf
                   default-directory)
               default-directory))))
     (list (ido-read-file-name "Find file: " default-directory) t)))
  (find-file file wildcards))

(provide 'davidc-ido)
