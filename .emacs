;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; default to better frame titles

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list
 'package-archives
 '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

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
 '(flyspell-issue-welcome-flag nil t)
 '(focus-follows-mouse nil)
 '(fringe-mode (quote (0)) nil (fringe))
 '(global-auto-complete-mode nil)
 '(indicate-buffer-boundaries (quote right))
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(ispell-dictionary "en_US")
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-program-name
   "C:/Program Files (x86)/Hunspell/hunspell-mingw-1.3.2-win32/bin/hunspell" t)
 '(keyboard-coding-system (quote utf-8-unix))
 '(mouse-wheel-mode t)
 '(package-selected-packages
   (quote
    (typescript-mode yaml-mode markdown-mode ox-pandoc ox-slimhtml ox-wk flymake-python-pyflakes flycheck-pycheckers darcula-theme csv cpputils-cmake company-dict cmake-project cmake-ide cmake-mode cargo flycheck-rust csv-mode gited dired-git-info diff-hl company)))
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
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;; FROM HERE ON DOWN, THIS IS CUSTOM

;; Do not save back up files.
(setq make-backup-files nil)

;; Black is the background colour
(invert-face 'default)

;; find some files, add to the "load-librapath".
(setq load-path (cons "~/emacsLisp" load-path))

;; Up/Down Case is not disabled!
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Set the default spell checking program to aspell.
(setq ispell-program-name "aspell")

;; Fix flyspell problem
(setq flyspell-issue-welcome-flag nil)

;; Sets Emacs to show the column and line number at the bottom bar.
(column-number-mode t)

;; Make org-mode default
(setq default-major-mode 'org-mode)

;; Use spaces, not tabs.
(setq-default indent-tabs-mode nil)

;; Display one tab character as N number of spaces; what is the tab
;; width? 4.
(setq-default tab-width 4)

;; ;; Hide back-up files in Dired.
;; ; Load Dired X when Dired is loaded.
;; (add-hook 'dired-load-hook '(lambda () (require 'dired-x)))
;; ; Turn on Omit mode.
;; (setq dired-omit-mode t)

;; Show Git information inside dired.
(with-eval-after-load 'dired
  (define-key dired-mode-map ")" 'dired-git-info-mode))

;; Windows Commands Scripts (.bat)
(autoload 'dos-mode "dos" "Edit Windows DOS scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

;; GLSL - OpenGL Shaders
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.ogsfx\\'" . glsl-mode))

;; Graphviz DOT Mode
(add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))
(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)

;; mel-mode - Autodesk Maya Embedded Language
(load-library '"$HOME/emacsLisp/mel-mode.el") ;; manually load the libray
(add-to-list 'auto-mode-alist '("\\.mel$" . mel-mode))
(autoload 'mel-mode "mel-mode" nil t)

;; rib-mode - RenderMan Interface Bytestream
(load-library '"$HOME/emacsLisp/rib-mode.el") ;; manually load the libray
(add-to-list 'auto-mode-alist '("\\.rib$" . rib-mode))
(autoload 'rib-mode "rib-mode" nil t)

;; rsl-mode - RenderMan Shading Language
(load-library '"$HOME/emacsLisp/rsl-mode.el") ;; manually load the libray
(autoload 'rsl-mode "rsl-mode" "RenderMan Shading Language" t)
(setq auto-mode-alist (append '(("\\.sl$" . rsl-mode)) auto-mode-alist))

;; Mathematica support (*.m files)
(load-file "$HOME/emacsLisp/wolfram-mode.el")
(autoload 'wolfram-mode "wolfram-mode" nil t)
(add-to-list 'auto-mode-alist '("\.m$" . wolfram-mode))

;; cmake-mode for "CMakeLists.txt" files.
(load-library '"$HOME/emacsLisp/cmake-mode") ;; manually load the libray
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

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
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(c-add-style "MyStyle"
             '("k&r"
               (c-basic-offset . 4)     ; Guessed value
               (c-offsets-alist
                (defun-block-intro . +) ; Guessed value
                (defun-close . 0)       ; Guessed value
                (innamespace . 0)       ; Guessed value
                (member-init-intro . ++) ; Guessed value
                (namespace-close . 0)   ; Guessed value
                (statement . 0)         ; Guessed value
                (topmost-intro . 0)     ; Guessed value
                (access-label . -)
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (arglist-close . c-lineup-close-paren)
                (arglist-cont c-lineup-gcc-asm-reg 0)
                (arglist-cont-nonempty . c-lineup-arglist)
                (arglist-intro . +)
                (block-close . 0)
                (block-open . 0)
                (brace-entry-open . 0)
                (brace-list-close . 0)
                (brace-list-entry . c-lineup-under-anchor)
                (brace-list-intro . +)
                (brace-list-open . 0)
                (c . c-lineup-C-comments)
                (case-label . 0)
                (catch-clause . 0)
                (class-close . 0)
                (class-open . 0)
                (comment-intro . c-lineup-comment)
                (composition-close . 0)
                (composition-open . 0)
                (cpp-define-intro c-lineup-cpp-define +)
                (cpp-macro . -1000)
                (cpp-macro-cont . +)
                (defun-open . 0)
                (do-while-closure . 0)
                (else-clause . 0)
                (extern-lang-close . 0)
                (extern-lang-open . 0)
                (friend . 0)
                (func-decl-cont . +)
                (inclass . +)
                (incomposition . +)
                (inexpr-class . +)
                (inexpr-statement . +)
                (inextern-lang . +)
                (inher-cont . c-lineup-multi-inher)
                (inher-intro . +)
                (inlambda . c-lineup-inexpr-block)
                (inline-close . 0)
                (inline-open . +)
                (inmodule . +)
                (knr-argdecl . 0)
                (knr-argdecl-intro . 0)
                (label . 0)
                (lambda-intro-cont . +)
                (member-init-cont . c-lineup-multi-inher)
                (module-close . 0)
                (module-open . 0)
                (namespace-open . 0)
                (objc-method-args-cont . c-lineup-ObjC-method-args)
                (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
                (objc-method-intro .
                                   [0])
                (statement-block-intro . +)
                (statement-case-intro . +)
                (statement-case-open . 0)
                (statement-cont . +)
                (stream-op . c-lineup-streamop)
                (string . -1000)
                (substatement . +)
                (substatement-label . 0)
                (substatement-open . 0)
                (template-args-cont c-lineup-template-args +)
                (topmost-intro-cont . c-lineup-topmost-intro-cont))))
(setq c-default-style "MyStyle"
      c-basic-offset 4)
(add-hook 'c-mode-common-hook
          (lambda ()
            (flyspell-prog-mode)
            (linum-mode 1)
            (cwarn-mode 1)
            (semantic-mode 1)
            (subword-mode 1)
            (imenu-add-to-menubar "Functions")
            (c-set-style "MyStyle")
            (c-toggle-electric-state 1)
            (c-toggle-auto-newline 0)
            (c-toggle-auto-hungry-state 0)
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

;; Rust Hook
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; Fullscreen
(defun fullscreen (&optional f)
  (interactive)
  (set-frame-parameter f 'fullscreen
                       (if (frame-parameter f 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)
;; (add-hook 'after-make-frame-functions 'fullscreen) ;; on startup
;; (global-set-key [ (f11) ] 'toggle-fullscreen) ;; won't work, WHY!?
(global-set-key "\C-f" 'compile)
;; (global-set-key [ (f1) ] 'previous-buffer)
;; (global-set-key [ (f2) ] 'next-buffer)

(global-set-key [ (f5) ] 'imenu)

;; Copy-Cut-Paste from clipboard with Super-C Super-X Super-V
(global-set-key (kbd "s-x") 'clipboard-kill-region) ;;cut
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ;;copy
(global-set-key (kbd "s-v") 'clipboard-yank) ;;paste

;; C/C++ - 'find other file' - Toggle between source and header file.
(global-set-key (kbd "C-M-o") 'ff-find-other-file)

;; Ctrl+tab mapped to Alt+tab
(define-key function-key-map [(control tab)] [?\M-\t])

;; Org-Mode customisation.
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "$HOME/org/work.org"
                             "$HOME/org/home.org"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
