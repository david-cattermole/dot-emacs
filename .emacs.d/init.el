;; .emacs.d/init.el

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; Move customization variables to a separate file and load it
(setq *custom-vars-file* (locate-user-emacs-file "custom-vars.el"))
(load *custom-vars-file* 'noerror 'nomessage)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Settings.

;; Use org-mode for new buffers.
(setq default-major-mode 'org-mode)

(custom-set-variables
 '(global-auto-complete-mode nil))

;; Don't show the Emacs toolbar or start-up screen.
(tool-bar-mode -1)
(custom-set-variables '(inhibit-startup-screen t))

(mouse-wheel-mode t)  ;; Allow use of mouse scroll wheel.
(display-time-mode t)  ;; Display the time at the bottom of Emacs.
(global-subword-mode t)  ;; Treat camelCase words as "camel" and "case".
(setq show-paren-mode t)   ;; Show highlights for matching brackets ().
(setq show-paren-style 'mixed)  ;; Highlight the matching parenthesis if it
                                ;; is visible, or the body otherwise.
(setq require-final-newline 'query)  ;; Ask the user to enter a newline or not.

;; How to auto-name conflicting buffer names.
(custom-set-variables
 '(uniquify-buffer-name-style 'forward nil (uniquify)))

;; The color theme of Emacs.
(custom-set-variables
 '(custom-enabled-themes '(tsdh-dark)))

;; Set up the fringe (sides of an Emacs buffer)
(custom-set-variables
 '(fringe-mode '(0) nil (fringe))
 '(indicate-buffer-boundaries 'right)
 '(indicate-empty-lines t))

(setq transient-mark-mode t)  ;; enable visual feedback on selections
(setq frame-title-format ;; Default to better frame titles
      (concat "%b - emacs@" (system-name)))
(setq diff-switches "-u")  ;; default to unified diffs
(setq make-backup-files nil)  ;; Do not save back up files.
(put 'downcase-region 'disabled nil) ;; Up/Down Case is not disabled!
(put 'upcase-region 'disabled nil)
(column-number-mode nil)  ;; Sets Emacs to show only the line
                          ;; number at the bottom bar.
(setq-default indent-tabs-mode nil)  ;; Use spaces, not tabs.
(setq confirm-kill-emacs 'y-or-n-p) ;; Emacs prompts me before I kill it.

;; Display one tab character as N number of spaces - what is the tab
;; width? 4.
(setq-default tab-width 4)

;; Set up Grep on Windows to use
(when (string-equal system-type "windows-nt") ; Microsoft Windows
  (setenv "PATH" (concat "C:\\GnuWin32\\bin;" (getenv "PATH")))
  (setq find-program "C:\\GnuWin32\\bin\\find.exe")
  (setq grep-program "C:\\GnuWin32\\bin\\grep.exe"))

;; Interactive Spelling Checker
;;
;; https://www.emacswiki.org/emacs/InteractiveSpell
;;
;; On Windows, the ezwinports project binaries seem to work well:
;; https://sourceforge.net/projects/ezwinports/
;;
;; On Windows, make sure to set the 'DICTIONARY' environment variable
;; to (for example) 'en_US'.
(setq flyspell-issue-welcome-flag nil)  ;; Fix flyspell problem

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
(load-library '"davidc")

;; Automatic highlighting of lines.
(add-hook 'find-file-hook 'davidc-highlight-it)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hotkeys

;; Easily reformat the current region of text, using different methods
;; for different major-modes.
(global-set-key [C-tab] 'davidc-format)

;; Navigation improvements using the arrow keys.
;;
;; This allows moving to the matching parentheses using "C-M-<up>" and
;; "C-M-<down>".
;;
;; We can also remap the normal "C-M-f" and "C-M-b" keys to the arrow
;; keys, so we can jump between sexpr (which is a language specific
;; concept).
;;
;; NOTE: "C-M-k" will kill the text between the matching
;; parentheses. "C-M-k" can be used in conjunction with "C-M-<up>" and
;; "C-M-<down>" to select then kill a block of code.
;;
;; Same as "C-M-n" - the default function is the same as "C-M-d".
(global-set-key (kbd "C-M-<down>") 'forward-list)
;; Same as "C-M-p" - the default function is the same as "C-M-u".
(global-set-key (kbd "C-M-<up>") 'backward-list)

;; Remapping "C-M-f" to "C-M-<right>".
(global-set-key (kbd "C-M-<right>") 'forward-sexp)
;; Remapping "C-M-b" to "C-M-<left>".
(global-set-key (kbd "C-M-<left>") 'backward-sexp)

;; Move text up or down.
(global-set-key (kbd "M-<down>") 'davidc-move-line-or-region-down)
(global-set-key (kbd "M-<up>") 'davidc-move-line-or-region-up)

;; Toggle `ls -1` (dash one) and `ls -l` (dash lower case L) output in
;; Dried.
(if (version< emacs-version "24.4")
    nil
  (progn
    (global-set-key (kbd "<f2>") 'dired-hide-details-mode)))

;; Go to previous/next buffer.
(global-set-key (kbd "<f3>") 'previous-buffer)
(global-set-key (kbd "<f4>") 'next-buffer)

;; Revert/Refresh the file in the active buffer.
(global-set-key (kbd "<f5>") 'revert-buffer)

;; Display and remove excessive whitespace.
(global-set-key (kbd "<f6>") 'whitespace-mode)
(global-set-key (kbd "<f7>") 'whitespace-cleanup)

;; Increase/decrease font size.
(global-set-key (kbd "<f9>") 'text-scale-decrease)
(global-set-key (kbd "<f12>") 'text-scale-increase)

;; Show the Interative Menu (IMenu) for the currently active buffer.
;; (global-set-key (kbd "<f8>") 'imenu)

;; Toggle Fullscreen.
(global-set-key (kbd "<f11>") 'davidc-fullscreen)

;; C/C++ - 'find other file' - Toggle between source and header file.
(global-set-key (kbd "C-M-o") 'ff-find-other-file)

;; 'Shift + Tab' (or 'C-x Tab') key will setup an interactive mode to
;; indent selected code.
(global-set-key (kbd "<backtab>") 'indent-rigidly)

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
(require 'mel-mode)
(add-to-list 'auto-mode-alist '("\\.mel$" . mel-mode))
(autoload 'mel-mode "mel-mode" nil t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; rib-mode - RenderMan Interface Bytestream
;; (load-library '"rib-mode") ;; manually load the libray
;; (add-to-list 'auto-mode-alist '("\\.rib$" . rib-mode))
;; (autoload 'rib-mode "rib-mode" nil t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; rsl-mode - RenderMan Shading Language
;; (load-library '"rsl-mode") ;; manually load the libray
;; (autoload 'rsl-mode "rsl-mode" "RenderMan Shading Language" t)
;; (setq auto-mode-alist (append '(("\\.sl$" . rsl-mode)) auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Mathematica support (*.m files)
;; (load-file "wolfram-mode")
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
            (linum-mode 1)))

;; Auto formatting with 'black'.
(require 'python-black)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C and C++ Hooks

(c-add-style "mmsolver"
             '((c-basic-offset . 4)
               (c-offsets-alist
                (knr-argdecl-intro . +)
                (substatement-open . +)
                (substatement-label . 2)
                (label . 2)
                (statement-case-open . 0)
                (inline-open . +)
                (brace-list-open . 0)
                (brace-list-intro . +)
                (topmost-intro-cont . c-lineup-topmost-intro-cont)
                (defun-open . 0)
                (class-open . 0)
                (class-close . 0)
                (inline-close . 0)
                (func-decl-cont . +)
                (knr-argdecl . 0)
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (member-init-intro . +)
                (member-init-cont . c-lineup-multi-inher)
                (inher-intro . +)
                (block-open . 0)
                (brace-list-close . 0)
                (brace-list-entry . 0)
                (brace-entry-open . 0)
                (statement-case-intro . +)
                (substatement . +)
                (case-label . 0)
                (access-label . -)
                (do-while-closure . 0)
                (else-clause . 0)
                (catch-clause . 0)
                (stream-op . c-lineup-streamop)
                (inclass . +)
                (cpp-macro-cont . +)
                (cpp-define-intro c-lineup-cpp-define +)
                (friend . 0)
                (objc-method-intro .
                                   [0])
                (objc-method-args-cont . c-lineup-ObjC-method-args)
                (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
                (extern-lang-open . 0)
                (namespace-open . 0)
                (module-open . 0)
                (composition-open . 0)
                (extern-lang-close . 0)
                (namespace-close . 0)
                (module-close . 0)
                (composition-close . 0)
                (inextern-lang . +)
                (innamespace . +)
                (inmodule . +)
                (incomposition . +)
                (template-args-cont c-lineup-template-args +)
                (inlambda . 0)
                (lambda-intro-cont . +)
                (inexpr-statement . +)
                (inexpr-class . +)
                (topmost-intro . 0)
                (defun-block-intro . +)
                (statement . 0)
                (statement-block-intro . +)
                (block-close . 0)
                (defun-close . 0)
                (statement-cont . +)
                (arglist-intro . +)
                (arglist-cont . 0)
                (c . c-lineup-C-comments)
                (inher-cont . c-lineup-multi-inher)
                (string . -1000)
                (comment-intro . c-lineup-comment)
                (arglist-cont-nonempty . c-lineup-arglist)
                (arglist-close . c-lineup-close-paren)
                (cpp-macro . -1000))))

(c-add-style "k&r_davidc"
             '("k&r"
               (c-basic-offset . 4)     ; Guessed value
               (c-offsets-alist
                (arglist-cont . 0)      ; Guessed value
                (arglist-intro . ++)    ; Guessed value
                (block-close . 0)       ; Guessed value
                (defun-block-intro . +) ; Guessed value
                (defun-close . 0)       ; Guessed value
                (statement . 0)         ; Guessed value
                (statement-block-intro . +) ; Guessed value
                (topmost-intro . 0)         ; Guessed value
                (access-label . -)
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (arglist-close . c-lineup-close-paren)
                (arglist-cont-nonempty . c-lineup-arglist)
                (block-open . 0)
                (brace-entry-open . 0)
                (brace-list-close . 0)
                (brace-list-entry . 0)
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
                (innamespace . +)
                (knr-argdecl . 0)
                (knr-argdecl-intro . 0)
                (label . 0)
                (lambda-intro-cont . +)
                (member-init-cont . c-lineup-multi-inher)
                (member-init-intro . +)
                (module-close . 0)
                (module-open . 0)
                (namespace-close . 0)
                (namespace-open . 0)
                (objc-method-args-cont . c-lineup-ObjC-method-args)
                (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
                (objc-method-intro .
                                   [0])
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

;; Treat header files ending with '.h' as C++ code, not C.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'c-mode-common-hook
          (lambda ()
            (flyspell-prog-mode)
            (linum-mode 1)
            (cwarn-mode 1)
            (semantic-mode 0)
            (subword-mode 1)
            (indent-tabs-mode . 0)
            (c-toggle-electric-state 1)
            (c-toggle-auto-newline 0)
            (c-toggle-auto-hungry-state 0)
            (c-toggle-syntactic-indentation 1)))


;; Auto-formatting with Clang-format.
(require 'clang-format)
(setq clang-format-style "file")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MEL Hook
(add-hook 'mel-mode-hook
          (lambda ()
            (flyspell-prog-mode)
            (linum-mode 1)
            (subword-mode 1)
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
(require 'dumb-jump)
(if (fboundp 'dumb-jump-xref-activate)
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
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
;; Once the *xref* opens, you can use the following bindings:
;; n or . = Next reference.
;; p or , = Previous reference.
;; TAB    = Display the reference on the current line and bury the *xref* buffer.
;; g      = Refresh the contents of *xref* buffer.
;; q      = Quit the *xref* buffer.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; project.el Setup - used to find and manage projects
;;
;; https://www.youtube.com/watch?v=vBQh2BeGhio
;;
;; C-x p p - Open a (or create) a Project.
;;
;; C-x p c - Compile the current project, using a command.
;;
;; C-x p k - Kill all buffers under the given project.
;;
;; C-x p D - Open Dired at project directory.
;;
;; C-x p e - Open eshell at project directory.
;;
;; C-x p s - Open shell at project directory.
;;
;; C-x p ! - Run project command.
;;
;; C-x p & - Run project command in the "background".
;;
;; C-x p ? - Show list of bindings starting with 'C-x p'
;;
(require 'project)
(add-hook 'project-find-functions 'davidc-git-project-finder)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String Inflection - https://github.com/akicho8/string-inflection/
;;
;; Convert selection to different case conventions.
;;
(require 'string-inflection)
(global-set-key (kbd "M-U") 'my-string-inflection-cycle-auto)
(global-set-key (kbd "M-L") 'string-inflection-toggle)

;; When t make string-inflection moves the cursor to the start of the
;; word.
(setq string-inflection-skip-backward-when-done t)

(defun davidc-string-inflection-toggle-function (str)
  "Not so much the case that in all caps when using normal foo_bar <--> fooBar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-lower-camelcase-function str))
   ((string-inflection-lower-camelcase-p str)
    (string-inflection-underscore-function str))
   (t
    (string-inflection-underscore-function str))))

(defun davidc-string-inflection-toggle ()
  "Toggle foo_bar <=> fooBar"
  (interactive)
  (string-inflection-insert
   (davidc-string-inflection-toggle-function (string-inflection-get-current-word))))

(defun davidc-string-inflection-cycle-auto ()
  "Switching case cylcing by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))

   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))

   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))

   ;; for elixir
   ((eq major-mode 'elixir-mode)
    (string-inflection-elixir-style-cycle))

   ;; default
   (t
    (string-inflection-python-style-cycle)))
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Perforce Version Control Integration - https://github.com/gareth-rees/p4.el
;; ;;
;; ;; This does not integrate with 'vc-mode', but rather provides a
;; ;; mapping of perforce commands to emacs commands.
;; ;;
;; ;; p4-add        (C-x v a)  Open file for add.
;; ;; p4-annotate   (C-x v V)  Annotate each line with the revision it was last updated.
;; ;; p4-client     (C-x v c)  Edit client workspace mapping.
;; ;; p4-edit       (C-x v e)  Open file for edit.
;; ;; p4-delete     (C-x v x)  Open file for delete.
;; ;; p4-diff       (C-x v =)  Diff local file against the depot.
;; ;; p4-filelog    (C-x v f)  Show revision history of file.
;; ;; p4-move       (C-x v m)  Move (rename) a file that’s open for edit.
;; ;; p4-opened     (C-x v o)  List open files.
;; ;; p4-reconcile  (C-x v z)  Reconcile client with workspace changes.
;; ;; p4-revert     (C-x v r)  Revert file, discarding local changes.
;; ;; p4-status     (C-x v s)  Identify differences between the workspace and the depot.
;; ;; p4-submit     (C-x v S)  Submit changes to the depot.
;; ;; p4-update     (C-x v g)  Get files from depot.
;; ;;
;; ;; Other cool commands are:
;; ;; p4-grep    - Grep search the perforce workspace for files containing a text string.
;; ;; p4-login   - Log into perforce.
;; ;; p4-changes - List all changelists that are open.
;; ;; p4-change  - Create a change list.
;; ;;
;; ;; The default global key binding prefix is "C-x p", but that
;; ;; conflicts with project.el.
;; (setq p4-global-key-prefix (kbd "C-x v"))
;; (require 'p4 )
