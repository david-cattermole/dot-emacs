;; .emacs

;;; uncomment for CJK utf-8 support for non-Asian users
;; (require 'un-define)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(baud-rate 19200)
 '(c-default-style
   '((c-mode . "mmsolver")
     (c++-mode . "mmsolver")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu")))
 '(custom-enabled-themes '(tsdh-dark))
 '(eol-mnemonic-dos "(DOS)")
 '(eol-mnemonic-mac "(Mac)")
 '(focus-follows-mouse nil)
 '(fringe-mode '(0) nil (fringe))
 '(global-auto-complete-mode nil)
 '(indicate-buffer-boundaries 'right)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(ispell-dictionary "en_US")
 '(ispell-highlight-face 'flyspell-incorrect)
 '(ispell-program-name
   "C:/Program Files (x86)/Hunspell/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe")
 '(keyboard-coding-system 'utf-8-unix)
 '(mouse-wheel-mode t)
 '(native-comp-async-report-warnings-errors nil)
 '(native-comp-warning-on-missing-source nil)
 '(reb-re-syntax 'rx)  ;; https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
 '(nxml-attribute-indent 4)
 '(nxml-child-indent 4)
 '(python-shell-prompt-detect-failure-warning nil)
 '(require-final-newline 'query)
 '(show-paren-mode t)
 '(speedbar-frame-parameters
   '((minibuffer)
     (width . 50)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0)))
 '(python-flymake-command '("C:/Program Files/ruff/ruff-x86_64-pc-windows-msvc-v0.0.256/ruff.exe" "check"
                            "--stdin-filename=<stdin>"
                            "-"))
 '(python-flymake-command-output-pattern
   '("^\\(?:<?stdin>?\\):\\(?1:[0-9]+\\):\\(?:\\(?2:[0-9]+\\):\\)? \\(?3:.*\\)$" 1 2 nil 3))
 '(python-flymake-msg-alist
   '(("\\(^redefinition\\|.*unused.*\\|used$\\)" . :warning)
    ("^E999" . :error)
    ("^[EW][0-9]+" . :note))
   )
 '(python-forward-sexp-function 'python-nav-forward-sexp)
 '(davidc-python-flymake-ruff-path
   "C:/Program Files/ruff/ruff-x86_64-pc-windows-msvc-v0.12.1/ruff.exe" t)
 '(python-black-command "C:/Program Files/ruff/ruff-x86_64-pc-windows-msvc-v0.12.1/ruff.exe")
 '(python-black-extra-args '("format"))
 '(davidc-config-add-gnuwin32-binaries nil)
 '(davidc-config-override-ispell-binary t)
 '(davidc-config-override-rg-binary nil)
 '(davidc-config-use-evil nil)
 '(davidc-config-use-p4 nil)
 '(davidc-config-use-bat-mode t)
 '(davidc-config-use-glsl-mode t)
 '(davidc-config-use-mel-mode t)
 '(davidc-config-use-rib-mode nil)
 '(davidc-config-use-rsl-mode nil)
 '(davidc-config-use-graphviz-dot-mode nil)
 '(davidc-config-use-cmake-mode t)
 '(davidc-config-use-rust-mode t)
 '(davidc-config-use-davidc-yaml-mode t)
 '(davidc-config-use-davidc-markdown-mode t)
 '(davidc-config-use-python-black t)
 '(davidc-config-use-python-flymake-ruff t)
 '(davidc-config-use-python-flymake-mypy t)
 '(davidc-config-use-clang-format t)
 '(davidc-config-use-flymake-clang-tidy t)
 '(davidc-config-use-flymake-rust-cargo-clippy t)
 '(davidc-config-use-flymake-jsonc t)
 '(davidc-config-use-flymake-yaml t)
 '(davidc-config-use-flymake-xml t)
 '(davidc-config-use-format t)
 '(davidc-config-use-rename-symbol t)
 '(davidc-config-use-region t)
 '(davidc-config-use-hideshow t)
 '(davidc-config-use-outline nil)
 '(davidc-config-use-dynamic-abbreviations t)
 '(davidc-config-use-string-inflection t)
 '(davidc-config-use-project t)
 '(davidc-config-use-dumb-jump t)
 '(davidc-config-use-rg t)
 '(davidc-config-use-davidc-find t)
 '(davidc-config-use-davidc-harpoon t)
 '(davidc-config-use-symbol-highlight t)
 '(davidc-config-use-server t)
 '(davidc-config-use-ibuffer t)
 '(davidc-config-use-icomplete nil)
 '(davidc-config-use-dape t)
 '(project-switch-commands
   '(
     ;; (project-find-file "Find file")
     ;; (project-find-regexp "Find regexp")
     ;; (project-find-dir "Find directory")
     (project-vc-dir "VC-Dir")
     (project-eshell "Eshell"))
   )
 '(speedbar-indentation-width 2)
 '(speedbar-use-images nil)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(warning-suppress-log-types
   '((comp)
     ((unlock-file))
     ((unlock-file))
     ((python python-shell-prompt-regexp))))
 '(warning-suppress-types '(((unlock-file)) ((python python-shell-prompt-regexp)))))

;; FROM HERE ON DOWN, THIS IS CUSTOM

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
