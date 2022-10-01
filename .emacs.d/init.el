;; .emacs.d/init.el

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Settings.

;; Use org-mode for new buffers.
(setq default-major-mode 'org-mode)

'(global-auto-complete-mode nil)

;; Don't show the Emacs toolbar or start-up screen.
(tool-bar-mode -1)
'(inhibit-startup-screen t)

(mouse-wheel-mode t)  ;; Allow use of mouse scroll wheel.
(display-time-mode t)  ;; Display the time at the bottom of Emacs.
(global-subword-mode t)  ;; Treat camelCase words as "camel" and "case".
(setq show-paren-mode t)   ;; Show highlights for matching brackets ().
(setq show-paren-style 'mixed)  ;; Highlight the matching parenthesis if it
                           ;; is visible, or the body otherwise.

(setq require-final-newline 'query)  ;; Ask the user to enter a newline or not.

;; How to auto-name conflicting buffer names.
'(uniquify-buffer-name-style 'forward nil (uniquify))

;; The color theme of Emacs.
(custom-set-variables
 '(custom-enabled-themes '(tsdh-dark)))

;; Set up the fringe (sides of an Emacs buffer)
'(fringe-mode '(0) nil (fringe))
'(indicate-buffer-boundaries 'right)
'(indicate-empty-lines t)

(setq transient-mark-mode t)  ;; enable visual feedback on selections
(setq frame-title-format ;; Default to better frame titles
      (concat "%b - emacs@" (system-name)))
(setq diff-switches "-u")  ;; default to unified diffs
(setq make-backup-files nil)  ;; Do not save back up files.
(put 'downcase-region 'disabled nil) ;; Up/Down Case is not disabled!
(put 'upcase-region 'disabled nil)
(setq ispell-program-name "aspell")  ;; Set the default spell checking
                                     ;; program to aspell.
(setq flyspell-issue-welcome-flag nil)  ;; Fix flyspell problem
(column-number-mode t)  ;; Sets Emacs to show the column and line
                        ;; number at the bottom bar.
(setq-default indent-tabs-mode nil)  ;; Use spaces, not tabs.
(setq confirm-kill-emacs 'y-or-n-p) ;; Emacs prompts me before I kill it.

;; Display one tab character as N number of spaces - what is the tab
;; width? 4.
(setq-default tab-width 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set default font
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas" t t)))
 ((string-equal system-type "darwin") ; MacOS
  (when (member "Menlo" (font-family-list))
    (set-frame-font "Menlo" t t)))
 ((string-equal system-type "gnu/linux") ; Linux
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-frame-font "DejaVu Sans Mono" t t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load libraries.

;; find some files, add to the library path.
(setq load-path (cons "~/.emacs.d/lisp/" load-path))

;; My custom functions.
(load-library '"davidc.el")

;; Automatic highlighting of lines.
(add-hook 'find-file-hook 'davidc-highlight-it)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hotkeys

;; Show the Interative Menu (IMenu) for the currently active buffer.
(global-set-key (kbd "<f2>") 'imenu)

;; Go to previous/next buffer.
(global-set-key (kbd "<f3>") 'previous-buffer)
(global-set-key (kbd "<f4>") 'next-buffer)

;; Revert/Refresh the file in the active buffer.
(global-set-key (kbd "<f5>") 'revert-buffer)

;; Display and remove excessive whitespace.
(global-set-key (kbd "<f6>") 'whitespace-mode)
(global-set-key (kbd "<f7>") 'whitespace-cleanup)

;; Toggle `ls -1` and `ls -l` output in Dried.
(if (version< emacs-version "24.4")
    nil
  (progn
    (global-set-key (kbd "<f8>") 'dired-hide-details-mode)))

;; ;; Lookup definition.
;; (global-set-key (kbd "<f7>") 'dictionary-lookup-definition)
;; (global-set-key (kbd "<f8>") 'davidc-lookup-word)
;; (global-set-key (kbd "<f9>") 'davidc-lookup-region)

;; Toggle Fullscreen.
(global-set-key (kbd "<f11>") 'davidc-fullscreen)

;; C/C++ - 'find other file' - Toggle between source and header file.
(global-set-key (kbd "C-M-o") 'ff-find-other-file)

;; ;; Press CTRL+RETURN to Compile
;; (global-set-key (kbd "C-<return>") 'compile)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize Dired.
(require 'dired )

;; Don't show full `ls` command output by default in Dired.
(if (version< emacs-version "24.4")
    nil
  (progn
    (add-hook 'dired-mode-hook 'davidc-dired-mode-details-setup)))

;; ;; Make dired use the same buffer for viewing directory.
;; (define-key dired-mode-map (kbd "RET")
;;   'dired-find-alternate-file) ;; was dired-advertised-find-file
;; (define-key dired-mode-map (kbd "^")
;;   (lambda ()
;;     (interactive)
;;     (find-alternate-file "..")))  ;; was dired-up-directory
;; (put 'dired-find-alternate-file 'disabled nil)

;; Jump to File in Dired
;;
;; In any open file, Alt+x dired-jump [Ctrl+x Ctrl+]to jump to the
;; directory of current buffer.
(if (version< emacs-version "28.1")
    nil
  (progn
    ;; for dired-jump
    (require 'dired-x)))

;; Revert Dired and other buffers.
;;
;; When files in a directory change, the buffer will update
;; automatically.
(setq global-auto-revert-non-file-buffers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows Commands Scripts (.bat)
(require 'bat-mode)
(autoload 'bat-mode "Edit Windows DOS scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . bat-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLSL - OpenGL Shaders
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.ogsfx\\'" . glsl-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Graphviz DOT Mode
;; (add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))
;; (autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mel-mode - Autodesk Maya Embedded Language
(load-library '"mel-mode.el") ;; manually load the libray
(add-to-list 'auto-mode-alist '("\\.mel$" . mel-mode))
(autoload 'mel-mode "mel-mode" nil t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; rib-mode - RenderMan Interface Bytestream
;; (load-library '"rib-mode.el") ;; manually load the libray
;; (add-to-list 'auto-mode-alist '("\\.rib$" . rib-mode))
;; (autoload 'rib-mode "rib-mode" nil t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; rsl-mode - RenderMan Shading Language
;; (load-library '"rsl-mode.el") ;; manually load the libray
;; (autoload 'rsl-mode "rsl-mode" "RenderMan Shading Language" t)
;; (setq auto-mode-alist (append '(("\\.sl$" . rsl-mode)) auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Mathematica support (*.m files)
;; (load-file "wolfram-mode.el")
;; (autoload 'wolfram-mode "wolfram-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\.m$" . wolfram-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cmake-mode for "CMakeLists.txt" files.
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text Hooks
(add-hook 'text-mode-hook 'turn-on-flyspell)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python Hook
(add-hook 'python-mode-hook
          (lambda ()
            (flyspell-prog-mode)
            (subword-mode 1)
            (linum-mode 1)
            (imenu-add-to-menubar "Functions")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C and C++ Hooks

(c-add-style "mmsolver"
             '("gnu"
               (c-basic-offset . 4)     ; Guessed value
               (c-offsets-alist
                (arglist-cont . 0)      ; Guessed value
                (arglist-intro . +)     ; Guessed value
                (block-close . 0)       ; Guessed value
                (defun-block-intro . +) ; Guessed value
                (defun-close . 0)       ; Guessed value
                (innamespace . 0)       ; Guessed value
                (namespace-close . 0)   ; Guessed value
                (statement . 0)         ; Guessed value
                (statement-block-intro . +) ; Guessed value
                (statement-cont . +)    ; Guessed value
                (topmost-intro . 0)     ; Guessed value
                (topmost-intro-cont . 0) ; Guessed value
                (access-label . -)
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (arglist-close . c-lineup-close-paren)
                (arglist-cont-nonempty . c-lineup-arglist)
                (block-open . 0)
                (brace-entry-open . 0)
                (brace-list-close . 0)
                (brace-list-entry . 0)
                (brace-list-intro first c-lineup-2nd-brace-entry-in-arglist c-lineup-class-decl-init-+ +)
                (brace-list-open . +)
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
                (inlambda . 0)
                (inline-close . 0)
                (inline-open . 0)
                (inmodule . +)
                (knr-argdecl . 0)
                (knr-argdecl-intro . 5)
                (label . 0)
                (lambda-intro-cont . +)
                (member-init-cont . c-lineup-multi-inher)
                (member-init-intro . +)
                (module-close . 0)
                (module-open . 0)
                (namespace-open . 0)
                (objc-method-args-cont . c-lineup-ObjC-method-args)
                (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
                (objc-method-intro . [0])
                (statement-case-intro . +)
                (statement-case-open . +)
                (stream-op . c-lineup-streamop)
                (string . -1000)
                (substatement . +)
                (substatement-label . 0)
                (substatement-open . +)
                (template-args-cont c-lineup-template-args +))))

;; Treat header files ending with '.h' as C++ code, not C.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'c-mode-common-hook
          (lambda ()
            (flyspell-prog-mode)
            (linum-mode 1)
            (cwarn-mode 1)
            (semantic-mode 1)
            (subword-mode 1)
            (imenu-add-to-menubar "Functions")
            (indent-tabs-mode . 0)
            (c-toggle-electric-state 1)
            (c-toggle-auto-newline 0)
            (c-toggle-auto-hungry-state 0)
            (c-toggle-syntactic-indentation 1)
            (c-set-style "mmsolver")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust Hook
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic Abbreviations (dabbrev).
;;
;; https://www.emacswiki.org/emacs/DynamicAbbreviations
;;
;; You can use "C-<return>" to on a word to auto-expand the word, and
;; if it's not correct you can use "C-<return>" again to cycle the
;; word list until you find what you want.
;;
;; By default this is set as "M-/".
(global-set-key (kbd "C-<return>") 'dabbrev-expand)
(define-key minibuffer-local-map (kbd "C-<return>") 'dabbrev-expand)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dumb-jump (https://github.com/jacktasia/dumb-jump)
;;
;; Put your cursor on a symbol and use "M-," to display a list of
;; suggestions.
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;; More details on Xref:
;;
;; M-. = Find definitions of an identifier (xref-find-definitions).
;;
;; M-, = Go back to where you previously invoked M-. and friends
;; (xref-pop-marker-stack).
;;
;; To find the definition of a specific identifier (i.e.: function,
;; method, class, etc) we can use "M-." which is bound to
;; xref-find-definitions and it will look for the identifier at point.
;;
;; To get back where you previously were, use "M-," which invokes
;; xref-pop-marker-stack and pops back to where M-. was last invoked.
;;
;; C-M-o = Jump to/from C++ source and C++ header file.
;;
