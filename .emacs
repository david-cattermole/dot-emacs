;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
(setq require-final-newline 'query)

;;; uncomment for CJK utf-8 support for non-Asian users
;; (require 'un-define)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(baud-rate 19200)
 '(column-number-mode t)
 '(default-major-mode (quote text-mode) t)
 '(display-time-mode t)
 '(eol-mnemonic-dos "(DOS)")
 '(eol-mnemonic-mac "(Mac)")
 '(flyspell-issue-welcome-flag nil)
 '(focus-follows-mouse nil)
 '(fringe-mode (quote (0)) nil (fringe))
 '(global-auto-complete-mode nil)
 '(indicate-buffer-boundaries (quote right))
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(inverse-video t)
 '(ispell-dictionary "british")
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-program-name "aspell")
 '(keyboard-coding-system (quote utf-8-unix))
 '(menu-bar-mode t)
 '(mouse-wheel-mode t)
 '(require-final-newline (quote query))
 '(show-paren-mode t)
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 50)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0))))
 '(speedbar-indentation-width 2)
 '(speedbar-use-images nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;; FROM HERE ON DOWN, THIS IS CUSTOM

;; find some files, add to the "load-librapath".
(setq load-path (cons "$HOME/emacsLisp" load-path))

;; Up/Down Case is not disabled!
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Set the default spell checking program to aspell.
(setq ispell-program-name "aspell")

;; Fix flyspell problem
(setq flyspell-issue-welcome-flag nil) 

;; Sets Emacs to show the column and line number at the bottom bar.
(column-number-mode t)

;; Make text-mode default
(setq default-major-mode 'org-mode)

;; Use spaces, not tabs.
(setq-default indent-tabs-mode nil)

;; Windows Commands Scripts (.bat)
(autoload 'dos-mode "dos" "Edit Windows DOS scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

;; Graphviz DOT Mode
(add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))
(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)

;; mel-mode - Autodesk Maya Embedded Language
(load-library '"$HOME/emacsLisp/mel-mode") ;; manually load the libray
(add-to-list 'auto-mode-alist '("\\.mel$" . mel-mode))
(autoload 'mel-mode "mel-mode" nil t)

;; rib-mode - RenderMan Interface Bytestream
(load-library '"$HOME/emacsLisp/rib-mode") ;; manually load the libray
(add-to-list 'auto-mode-alist '("\\.rib$" . rib-mode))
(autoload 'rib-mode "rib-mode" nil t)

;; rsl-mode - RenderMan Shading Language
(load-library '"$HOME/emacsLisp/rsl-mode") ;; manually load the libray
(setq auto-mode-alist (append '(("\\.sl$" . rsl-mode)) auto-mode-alist))
(autoload 'rsl-mode "rsl-mode" "RenderMan Shading Language" t)

;; Mathematica support (*.m files)
(load-file "$HOME/emacsLisp/wolfram-mode.el")
(autoload 'wolfram-mode "wolfram-mode" nil t)
(add-to-list 'auto-mode-alist '("\.m$" . wolfram-mode))

;; cmake-mode for "CMakeLists.txt" files.
(if (eq system-type 'gnu/linux)
    ((setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))
     (autoload 'cmake-mode "/usr/share/cmake/editors/emacs/cmake-mode.el" t))
)

;; Task Juggler Project Files.
(load-library '"$HOME/emacsLisp/taskjuggler-mode") ;; manually load the libray
(autoload 'tjp-mode "tjp-mode" "Task Juggler Project" t)
(setq auto-mode-alist (append '(("\\.tjp$" . tjp-mode)) auto-mode-alist))
(autoload 'tjp-mode "tjp-mode" nil t)

;; Text Hooks
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Python Hook
(add-hook 'python-mode-hook 
	  (lambda () 
	    (flyspell-prog-mode)
	    (subword-mode 1)
	    (linum-mode 1)
	    (imenu-add-to-menubar "Functions")))

;; C and C++ Hook
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (flyspell-prog-mode)
	    (linum-mode 1)
	    (cwarn-mode 1)
	    (semantic-mode 1)
	    (subword-mode 1)
	    (imenu-add-to-menubar "Functions")
	    (c-set-style "k&r")
	    (c-toggle-electric-state 1)
	    (c-toggle-auto-newline 0)
	    (c-toggle-auto-hungry-state 1)
	    (c-toggle-syntactic-indentation 1)))

;; MEL Hook
(add-hook 'mel-mode-hook
	  (lambda ()
	    (flyspell-prog-mode)
	    (linum-mode 1)
	    (subword-mode 1)
	    (imenu-add-to-menubar "Functions")
	    (c-toggle-electric-state 1)
	    (c-toggle-auto-newline 0)
	    (c-toggle-auto-hungry-state 1)
	    (c-toggle-syntactic-indentation 1)))

;; Fullscreen
(defun fullscreen (&optional f)
  (interactive)
  (set-frame-parameter f 'fullscreen
		       (if (frame-parameter f 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)
;; (add-hook 'after-make-frame-functions 'fullscreen) ;; on startup
;; (global-set-key [ (f11) ] 'toggle-fullscreen) ;; won't work, WHY!?
(global-set-key "\C-f" 'compile)
(global-set-key [ (f1) ] 'previous-buffer)
(global-set-key [ (f2) ] 'next-buffer)

;; Copy-Cut-Paste from clipboard with Super-C Super-X Super-V
(global-set-key (kbd "s-x") 'clipboard-kill-region) ;;cut
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ;;copy
(global-set-key (kbd "s-v") 'clipboard-yank) ;;paste

;; Ctrl+tab mapped to Alt+tab
(define-key function-key-map [(control tab)] [?\M-\t])

;; Org-Mode customisation.
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "$HOME/org/work.org"
                             "$HOME/org/home.org"))
