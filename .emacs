;; .emacs
;; Time-stamp: <2014-01-05 18:34:08 davidc>

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
 '(ispell-dictionary "british")
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-program-name "aspell")
 '(keyboard-coding-system (quote utf-8-unix))
 '(menu-bar-mode t)
 '(mouse-wheel-mode t)
 '(require-final-newline (quote query))
 '(show-paren-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 87 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

;; FROM HERE ON DOWN, THIS IS CUSTOM

;; find some files, add to the "load-librapath".
(setq load-path (cons "$HOME/emacsLisp" load-path))

;; Up/Down Case is not disabled!
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Set the default spell checking program to aspell,
;; also set the default dictionary to 'british'.
(setq ispell-program-name "aspell"
      ispell-dictionary "british")

;; Fix flyspell problem
(setq flyspell-issue-welcome-flag nil) 

;; turn on type-break-mode
;; this will enable you to take
;; forced typing breaks (to help stop R.S.I)
;; (type-break-mode)

;; sets Emacs to show the column and line number at the bottom bar.
(column-number-mode t)

;; make text-mode default
(setq default-major-mode 'text-mode)

;; This is so that I can edit *.bat files (DOS Scripts)
(autoload 'dos-mode "dos" "Edit Dos scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

;; ;; follow-mode allows easier editing of long files
;; (follow-mode t)

;; ;; turns on line numbers of the left hand side of a buffer.
;; ;; (linum-mode t)
;; (global-linum-mode t)

;; editing based on visual lines
;; (global-visual-line-mode t)

;; Set Subword as default
;; (global-subword-mode t)

;; ;; Code Folding (using "folding")
;; (load "folding" 'nomessage 'noerror)
;; (folding-mode-add-find-file-hook)

;; mel-mode hook
(load-library '"$HOME/emacsLisp/mel-mode") ;; manually load the libray
(add-to-list 'auto-mode-alist '("\\.mel$" . mel-mode))
(autoload 'mel-mode "mel-mode" nil t)

;; rib-mode 
(load-library '"$HOME/emacsLisp/rib-mode") ;; manually load the libray
(add-to-list 'auto-mode-alist '("\\.rib$" . rib-mode))
(autoload 'rib-mode "rib-mode" nil t)

;; rsl-mode
;; (add-to-list 'auto-mode-alist '("\\.sl$" . rsl-mode))
;; (autoload 'rsl-mode "rsl-mode" nil t)
(load-library '"$HOME/emacsLisp/rsl-mode") ;; manually load the libray
(autoload 'rsl-mode "rsl-mode" "RenderMan Shading Language Editing Mode" t)
(setq auto-mode-alist (append '(("\\.sl$" . rsl-mode)) auto-mode-alist))
(autoload 'rsl-mode "rsl-mode" nil t)

;; ;; Newest version of python-mode
;; (add-to-list 'load-path "$HOME/bin/emacsLisp/python-mode-6.1.2/") 
;; (setq py-install-directory "$HOME/bin/emacsLisp/python-mode-6.1.2/")
;; (require 'python-mode)

;; Text Hooks
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; ;; Python Hooks
;; (add-hook 'python-mode-hook 'flyspell-prog-mode)
;; (add-hook 'python-mode-hook 'linum-mode)
;; ;; (add-hook 'python-mode-hook 'hs-minor-mode) ;; Hide/Show is not very good in Python.

;; Python Hook
(add-hook 'python-mode-hook 
	  (lambda () 
	    (flyspell-prog-mode)
	    (linum-mode 1)
	    (imenu-add-to-menubar "Functions")))

;; C Hook
(add-hook 'c-mode-hook 
	  (lambda () 
	    (flyspell-prog-mode)
	    (linum-mode 1)
	    (hs-minor-mode 1)
	    (imenu-add-to-menubar "Functions")
	    (c-toggle-electric-state 1)
	    (c-toggle-auto-newline 0)
	    (c-toggle-auto-hungry-state 1)
	    (c-toggle-syntactic-indentation 1)))

;; C++ Hook
(add-hook 'c++-mode-hook 
	  (lambda () 
	    (flyspell-prog-mode)
	    (linum-mode 1)
	    (hs-minor-mode 1)
	    (imenu-add-to-menubar "Functions")
	    (c-toggle-electric-state 1)
	    (c-toggle-auto-newline 0)
	    (c-toggle-auto-hungry-state 1)
	    (c-toggle-syntactic-indentation 1)))

;; MEL Hook
(add-hook 'mel-mode-hook 
	  (lambda () 
	    (flyspell-prog-mode)
	    (linum-mode 1)
	    (hs-minor-mode 1)
	    (imenu-add-to-menubar "Functions")
	    (c-toggle-electric-state 1)
	    (c-toggle-auto-newline 0)
	    (c-toggle-auto-hungry-state 1)
	    (c-toggle-syntactic-indentation 1)))

;; Emacs For Python
;; (load-file "$HOME/bin/emacsLisp/emacsForPython/epy-init.el")

(global-set-key "\C-f" 'compile)
(global-set-key [ (f1) ] 'previous-buffer)
(global-set-key [ (f2) ] 'next-buffer)
;; (global-set-key [ (f11) ] 'toggle-fullscreen) ;; won't work, WHY!?

;; Copy-Cut-Paste from clipboard with Super-C Super-X Super-V
(global-set-key (kbd "s-x") 'clipboard-kill-region) ;;cut
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ;;copy
(global-set-key (kbd "s-v") 'clipboard-yank) ;;paste

;; Ctrl+tab mapped to Alt+tab
(define-key function-key-map [(control tab)] [?\M-\t])

;; Timestamp
(add-hook 'before-save-hook 'time-stamp)

;; HideShow shortcuts.
;; Note: We could have problems overriding keys, 
;;  if so perhaps use this as reference:
;;  http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(global-set-key "\C-h" 'hs-toggle-hiding)
;; (global-set-key "\C-h" 'hs-hide-block)
;; (global-set-key "\C-H" 'hs-show-block)

;; ;; Arrow Keys
;; (global-set-key "\eOA" 'previous-line) ;; Up Arrow
;; (global-set-key "\eOB" 'next-line) ;; Down Arrow
;; (global-set-key "\eOC" 'subword-forward) ;; Right Arrow, default is forward-word
;; (global-set-key "\eOD" 'subword-backward) ;; Left Arrow, default is backward-word

;; remove the toolbar from the GUI
;; (tool-bar-mode nil)

;; ;; Startup the Speedbar
;; (speedbar-frame-mode)

;; ;; run a shell
;; (shell)
