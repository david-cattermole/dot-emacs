;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RenderMan Shader Mode
;; c-mode + RenderMan compile/render functionality
;; steve may, accad, 4/27/95
;;   initial version
;; steve may, accad, 4/27/96
;;   - added capability to set RIB filename and included directories
;;   - added highlighting capability
;;
;;----------------------------------------
;;
;; To make this mode enabled whenever a file ending in ".sl" is loaded,
;; add the following to your .emacs file.
;;
;;(autoload 'rsl-mode "rsl-mode" "RenderMan Shading Language Editing Mode" t)
;;(setq auto-mode-alist (append '(("\\.sl$" . rsl-mode)) auto-mode-alist))
;;
;;----------------------------------------
;;
;; to turn on highlighting, add the following to your .emacs. 
;; See hilit19.el for more info on the highlighting mode.
;;
;; (cond (window-system
;;       (setq hilit-mode-enable-list  '(not text-mode)
;;	     hilit-background-mode   'light
;;	     hilit-inhibit-hooks     nil
;;	     hilit-inhibit-rebinding nil)
;;       (require 'hilit19)
;;       ))
;;

(defun rsl-mode ()
  "Major mode for editing RenderMan shaders.
This is actually just C mode with commands for compiling and 
rendering shaders.

C-c C-c    save buffer & compile shader for PhotoRealistic RenderMan
C-c C      save buffer & compile shader for BMRT
C-c C-r    call render with the current RIB filename
C-c R      call rendrib with the current RIB filename
C-c C-s    set the current RIB filename (default is rman.rib)
C-c C-i    set the current include directories as LISP list of strings;
           each string denoting one directory. For example (at the prompt):
           (\"/usr/local/shaders\" \"/usr/shaders\").
C-c C-l    load most recently loaded AL file (via emacs) back into OX

To get info on C mode, select 'Describe Function...' from the 'Help'
menu and enter 'c-mode' at the prompt.
"
  (interactive)
  (c-mode)
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'rsl-compile)
    (define-key map "\C-cC" 'rsl-compile-bmrt)
    (define-key map "\C-c\C-r" 'rsl-render)
    (define-key map "\C-cR" 'rsl-render-bmrt)
    (define-key map "\C-c\C-s" 'rsl-set-ribfile)
    (define-key map "\C-c\C-i" 'rsl-set-inc-dirs)
    (define-key map "\C-c\C-l" 'rsl-load-AL)
    (use-local-map map))
  (setq major-mode 'rsl-mode)
  (setq mode-name "RenderMan Shader"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save & compile current buffer using shader (PRMan)
;;
(defvar rsl-mode-inc-list nil "List of shader compiler include directories.")

(defvar rsl-mode-rib-file "rman.rib" "Name of RIB file used for shader tests.")

(defun rsl-exp-inc-list (alist)
  (cond
   ((null alist) "")
   (t (concat " -I" (expand-file-name (car alist)) 
	      (rsl-exp-inc-list (cdr alist))))))

(defun rsl-compile (args)
  "Save the RenderMan shader in the current buffer and compile it for PhotoRealistic RenderMan."
  (interactive "P")
  (save-buffer)
  (let* ((rsl-source-file (buffer-name)))
    (compile (concat "shader" (rsl-exp-inc-list rsl-mode-inc-list) 
		     " " rsl-source-file)))
  (message "Type C-x 1 to remove compilation window."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save & compile current buffer using slc (BMRT)
;;
(defun rsl-set-ribfile (name)
  "Set the name of the rib file used for render tests."
  (interactive "sName of RIB file: ")
  (setq rsl-mode-rib-file name))

(defun rsl-set-inc-dirs (dirs)
  "Set the current include directories as a LISP list of strings - each string denoting one directory."
  (interactive "xList of directories to include: ")
  (setq rsl-mode-inc-list dirs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save & compile current buffer using slc (BMRT)
;;
(defun rsl-compile-bmrt (args)
  "Save the RenderMan shader in the current buffer and compile it for BMRT."
  (interactive "P")
  (save-buffer)
  (let* ((rsl-source-file (buffer-name)))
    (compile (concat "slc" (rsl-exp-inc-list rsl-mode-inc-list) 
		     " " rsl-source-file)))
  (message "Type C-x 1 to remove compilation window."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; execute shell command "render rman.rib"
;;
(defun rsl-render (args)
  "Render the current RIB file."
  (interactive "P")
  (let ((cmd (concat "render " rsl-mode-rib-file)))
    (message cmd)
    (shell-command cmd '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; execute shell command "rendrib rman.rib"
;;
(defun rsl-render-bmrt (args)
  "Render the RIB file rman.rib in the current directory."
  (interactive "P")
  (let ((cmd (concat "rendrib " rsl-mode-rib-file)))
    (message cmd)
    (shell-command cmd '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load most recently loaded AL file (depends on free variable
;; al-mode 'al-mode-mrf'). If al-mode-mrf is nil, then load AL file
;; with same name as current buffer (change to extension to .al)
;;
(defun rsl-load-AL (args)
  "Load the most recently loaded AL (Animation Language) file into OX."
  (interactive "P")
  (let (
	(default (concat 
		  (substring (buffer-name) 0 (- (length (buffer-name)) 3))
		  ".al"))
	al-file
	)
    (setq al-file
	  (if (boundp 'al-mode-mrf)
	      (if (null al-mode-mrf) default al-mode-mrf)
	    default))
    (message (concat "sending message to OX to load " al-file "..."))
    (shell-command (concat "oxmsg \"(load '" al-file ")\"") '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlighting for hilit19 functionality
;;

(if (fboundp 'hilit-set-mode-patterns)
    (hilit-set-mode-patterns
     'rsl-mode
     '(
       ("/\\*" "\\*/" comment)
       (hilit-string-find ?' string)
       ("^#[ \t]*\\(undef\\|define\\).*$" "[^\\]$" define)
       ("^#.*$" nil include)

       ;; datatypes
       ("[^_]\\<\\(\\(\\(varying\\|uniform\\)[ \n\t]+\\)?\\(float\\|color\\|point\\|string\\|vector\\)\\)" nil type)
       
       ;; key words
       ("[^_]\\<\\(return\\|if\\|else\\|break\\|continue\\|while\\|for\\)\\>[^_]" 1 keyword)
       ("[^_]\\<\\(illuminance\\|illuminate\\|solar\\)\\>[^_]" 1 keyword)
       ("[^_]\\<\\(radians\\|degrees\\|sin\\|asin\\|cos\\|acos\\|tan\\|atan\\|pow\\|exp\\|sqrt\\|log\\|mod\\|abs\\|sign\\|min\\|max\\|clamp\\|floor\\|ceil\\|round\\|step\\|smoothstep\\|spline\\|Du\\|Dv\\|Deriv\\|random\\|noise\\)\\>[^_]" 1 keyword)
       ("[^_]\\<\\(xcomp\\|ycomp\\|zcomp\\|setxcomp\\|setycomp\\|setzcomp\\|length\\|distance\\|area\\|normalize\\|faceforward\\|reflect\\|refract\\|fresnel\\|transform\\|depth\\|calculatenormal\\)\\>[^_]" 1 keyword)
       ("[^_]\\<\\(comp\\|setcomp\\|mix\\)\\>[^_]" 1 keyword)
       ("[^_]\\<\\(ambient\\|diffuse\\|specular\\|phong\\|trace\\)\\>[^_]" 1 keyword)
       ("[^_]\\<\\(texture\\|environment\\|bump\\|shadow\\|incident\\|opposite\\)\\>[^_]" 1 keyword)
       ("[^_]\\<\\(printf\\)\\>[^_]" 1 keyword)
       ("[^_]\\<\\(surface\\|light\\|interior\\|exterior\\|atmosphere\\|displacement\\|imager\\|transformation\\)\\>[^_]" 1 keyword)
       ("[^_]\\<\\(output\\|inversesqrt\\|vtransform\\|ntransform\\|ctransform\\|rotate\\|filterstep\\lightsource\\|ptlined\\|pnoise\\)\\>[^_]" nil keyword)

       ;; globals
       ("[^_]\\<\\(Cs\\|Os\\|P\\|dPdu\\|dPdv\\|N\\|Ng\\|u\\|v\\|du\\|dv\\|s\\|t\\|L\\|Cl\\|I\\|Ci\\|Oi\\|E\\|ncomps\\|time\\|Ps\\|Ol\\|alpha\\)\\>[^_]" 1 label)
       )))
