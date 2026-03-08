;;; build.el --- Byte-compile all Emacs Lisp packages -*- lexical-binding: t; -*-

;;; Commentary:
;; Byte-compiles every .el file in .emacs.d/lisp in dependency order so that
;; each feature is fully compiled before its dependents are processed,
;; keeping spurious "reference to free variable" warnings to a minimum.
;;
;; Usage:
;;   ./build.sh                      (Linux/macOS — recommended)
;;   EMACS=/path/to/emacs ./build.sh (override the Emacs binary)
;;   emacs --batch -l /path/to/build.el
;;
;; How it works:
;;   1. All package sub-directories under .emacs.d/lisp/ are added to
;;      load-path so that (require ...) calls inside compiled files resolve.
;;   2. Every .el file in `davidc-build--files' is compiled in the order
;;      listed (Tier 1 → Tier 5, then evil, magit, dape, and the custom
;;      davidc-*.el libraries).  The ordering ensures each file's
;;      dependencies are compiled first.
;;   3. Missing files are skipped (SKIP) rather than aborting the build.
;;   4. A summary of compiled / skipped / error counts is printed and the
;;      process exits with status 1 if any file failed to compile.

;;; Code:

(require 'bytecomp)
(require 'cl-lib)

;; ---------------------------------------------------------------------------
;; Directory layout

(defvar davidc-build--repo-root
  (file-name-directory
   (expand-file-name (or load-file-name buffer-file-name "build.el")))
  "Root directory of the dot-emacs repository (contains build.el).")

(defvar davidc-build--lisp-dir
  (expand-file-name ".emacs.d/lisp" davidc-build--repo-root)
  "Top-level lisp directory (.emacs.d/lisp).")

;; All sub-directories that contain Elisp packages.
(defvar davidc-build--package-dirs
  '("compat" "evil" "magit" "my-common" "my-modes" "my-tools"
    "p4" "project" "rg" "rust-mode" "wgrep" "ztree")
  "Package sub-directories relative to `davidc-build--lisp-dir'.")

;; Extend load-path so every (require ...) inside compiled files resolves.
(cl-pushnew davidc-build--lisp-dir load-path :test #'equal)
(dolist (dir davidc-build--package-dirs)
  (cl-pushnew (expand-file-name dir davidc-build--lisp-dir) load-path :test #'equal))

;; ---------------------------------------------------------------------------
;; Compilation list — paths relative to `davidc-build--lisp-dir', in dependency order.
;; Test files and auto-generated autoload files are intentionally excluded.

(defvar davidc-build--files
  '(
    ;; ----------------------------------------------------------------
    ;; compat  (no local deps)
    ;; ----------------------------------------------------------------
    "compat/compat-macs.el"
    "compat/compat-25.el"
    "compat/compat-26.el"
    "compat/compat-27.el"
    "compat/compat-28.el"
    "compat/compat-29.el"
    "compat/compat-30.el"
    "compat/compat.el"

    ;; ----------------------------------------------------------------
    ;; magit helpers  (only require compat)
    ;; ----------------------------------------------------------------
    "magit/cond-let.el"
    "magit/llama.el"                    ; requires: compat

    ;; ----------------------------------------------------------------
    ;; my-common base  (built-in deps only)
    ;; ----------------------------------------------------------------
    "my-common/dash.el"
    "my-common/s.el"
    "my-common/goto-chg.el"
    "my-common/popup.el"                ; requires: cl-lib, mule

    ;; ----------------------------------------------------------------
    ;; project  (built-in deps only)
    ;; ----------------------------------------------------------------
    "project/project.el"

    ;; ----------------------------------------------------------------
    ;; wgrep base  (requires grep — built-in)
    ;; ----------------------------------------------------------------
    "wgrep/wgrep.el"

    ;; ----------------------------------------------------------------
    ;; rg base files  (built-in deps only)
    ;; ----------------------------------------------------------------
    "rg/rg-header.el"                   ; requires: mouse
    "rg/rg-history.el"                  ; requires: cl-lib, subr-x
    "rg/rg-info-hack.el"

    ;; ----------------------------------------------------------------
    ;; my-tools base  (built-in deps only)
    ;; ----------------------------------------------------------------
    "my-tools/reformatter.el"           ; requires: cl-lib, ansi-color
    "my-tools/clang-format.el"          ; requires: cl-lib, xml
    "my-tools/string-inflection.el"

    ;; ----------------------------------------------------------------
    ;; my-modes  (built-in deps only)
    ;; ----------------------------------------------------------------
    "my-modes/bat-mode.el"
    "my-modes/cmake-mode.el"            ; requires: rst, rx, thingatpt
    "my-modes/glsl-mode.el"             ; requires: cc-mode, find-file, align
    "my-modes/graphviz-dot-mode.el"     ; requires: easymenu
    "my-modes/mel-mode.el"              ; requires: font-lock, cc-mode, browse-url
    "my-modes/rib-mode.el"
    "my-modes/rsl-mode.el"

    ;; ----------------------------------------------------------------
    ;; rust-mode base  (built-in deps only)
    ;; ----------------------------------------------------------------
    "rust-mode/rust-rustfmt.el"
    "rust-mode/rust-compile.el"         ; requires: compile
    "rust-mode/rust-cargo.el"           ; requires: json
    "rust-mode/rust-playpen.el"

    ;; ----------------------------------------------------------------
    ;; ztree base  (built-in deps only)
    ;; ----------------------------------------------------------------
    "ztree/ztree-util.el"
    "ztree/ztree-protocol.el"           ; requires: cl-lib

    ;; ----------------------------------------------------------------
    ;; p4  (built-in deps only)
    ;; ----------------------------------------------------------------
    "p4/p4.el"

    ;; ================================================================
    ;; Tier 2 — require Tier-1 local packages
    ;; ================================================================

    "my-common/dash-functional.el"      ; requires: dash
    "my-common/transient.el"            ; requires: compat, cond-let, eieio, …
    "my-tools/dumb-jump.el"             ; requires: s, dash, popup
    "my-tools/python-black.el"          ; requires: dash, reformatter

    ;; wgrep adapters — all require wgrep
    "wgrep/wgrep-rg.el"
    "wgrep/wgrep-ack.el"
    "wgrep/wgrep-ag.el"
    "wgrep/wgrep-deadgrep.el"
    "wgrep/wgrep-helm.el"
    "wgrep/wgrep-pt.el"

    "rust-mode/rust-common.el"          ; requires: rust-rustfmt
    "ztree/ztree-view.el"               ; requires: ztree-util, ztree-protocol
    "ztree/ztree-diff-model.el"         ; requires: ztree-util

    ;; ================================================================
    ;; Tier 3 — require Tier-2 local packages
    ;; ================================================================

    "rg/rg-result.el"                   ; requires: rg-header, rg-history, wgrep-rg
    "rust-mode/rust-mode.el"            ; requires: rust-common (+ rust-cargo, rust-compile, rust-playpen, rust-rustfmt)
    "ztree/ztree-dir.el"                ; requires: ztree-util, ztree-view, ztree-protocol
    "ztree/ztree-diff.el"               ; requires: ztree-view, ztree-diff-model

    ;; ================================================================
    ;; Tier 4 — require Tier-3 local packages
    ;; ================================================================

    "rg/rg-ibuffer.el"                  ; requires: rg-result
    "rg/rg-menu.el"                     ; requires: rg-result, transient
    "rust-mode/rust-utils.el"           ; requires: rust-mode, thingatpt
    "rust-mode/rust-mode-treesitter.el" ; requires: rust-mode
    "rust-mode/rust-prog-mode.el"       ; requires: rust-mode
    "ztree/ztree.el"                    ; requires: ztree-dir, ztree-diff

    ;; ================================================================
    ;; Tier 5 — rg top-level
    ;; ================================================================

    "rg/rg.el"                          ; requires: rg-ibuffer, rg-menu, rg-result, rg-info-hack
    "rg/rg-isearch.el"                  ; requires: rg

    ;; ================================================================
    ;; evil  (requires goto-chg from my-common)
    ;; Compiled in strict dependency order to avoid forward-reference
    ;; warnings.
    ;; ================================================================

    "evil/evil-vars.el"
    "evil/evil-digraphs.el"             ; requires: evil-vars
    "evil/evil-common.el"               ; requires: evil-vars, evil-digraphs
    "evil/evil-core.el"                 ; requires: evil-common
    "evil/evil-states.el"               ; requires: evil-core
    "evil/evil-repeat.el"               ; requires: evil-states
    "evil/evil-macros.el"               ; requires: evil-common, evil-states, evil-repeat
    "evil/evil-types.el"                ; requires: evil-common, evil-macros
    "evil/evil-ex.el"                   ; requires: evil-common, evil-states, evil-types
    "evil/evil-search.el"               ; requires: evil-core, evil-common, evil-ex
    "evil/evil-commands.el"             ; requires: evil-common, evil-digraphs, evil-search, evil-states, evil-ex
    "evil/evil-jumps.el"                ; requires: evil-core, evil-states
    "evil/evil-command-window.el"       ; requires: evil-vars, evil-common, evil-search, evil-ex
    "evil/evil-maps.el"                 ; requires: evil-states, evil-ex, evil-commands, evil-command-window
    "evil/evil-integration.el"          ; requires: evil-maps, evil-core, evil-macros, evil-types, evil-repeat
    "evil/evil-keybindings.el"          ; requires: evil-maps, evil-core, evil-macros, evil-types, evil-repeat
    "evil/evil.el"

    ;; ================================================================
    ;; magit  (complex inter-dependencies; compiled bottom-up)
    ;; ================================================================

    ;; Foundation
    "magit/magit-section.el"            ; requires: compat, cond-let, llama
    "magit/magit-base.el"               ; requires: magit-section, compat, cond-let, llama
    "magit/magit-autorevert.el"         ; requires: autorevert (built-in)
    "magit/magit-git.el"                ; requires: magit-base
    "magit/magit-mode.el"               ; requires: magit-base, magit-git
    "magit/with-editor.el"              ; requires: compat, server, shell
    "magit/magit-process.el"            ; requires: magit-base, magit-git, magit-mode
    "magit/magit-transient.el"          ; requires: magit-git, magit-mode, magit-process, transient
    "magit/magit-margin.el"             ; requires: magit-base, magit-transient, magit-mode
    "magit/magit-core.el"               ; requires: all of the above
    ;; git-commit before magit-diff (magit-diff requires it)
    "magit/git-commit.el"               ; requires: magit-git, magit-mode, magit-process, with-editor, transient
    "magit/magit-diff.el"               ; requires: magit-core, git-commit
    "magit/magit-log.el"                ; requires: magit-core, magit-diff
    "magit/magit-reflog.el"             ; requires: magit-core, magit-log
    "magit/magit-wip.el"                ; requires: magit-core, magit-log
    "magit/magit-apply.el"              ; requires: magit-core, magit-diff, magit-wip
    "magit/magit-repos.el"              ; requires: magit-core
    ;; magit.el — top-level entry point
    "magit/magit.el"
    ;; Everything below requires magit
    "magit/magit-reset.el"
    "magit/magit-sequence.el"
    "magit/magit-commit.el"             ; requires: magit, magit-sequence
    "magit/magit-remote.el"
    "magit/magit-clone.el"
    "magit/magit-fetch.el"
    "magit/magit-pull.el"
    "magit/magit-push.el"
    "magit/magit-merge.el"              ; requires: magit, magit-diff
    "magit/magit-tag.el"
    "magit/magit-worktree.el"
    "magit/magit-notes.el"
    "magit/magit-patch.el"
    "magit/magit-stash.el"              ; requires: magit, magit-reflog, magit-sequence
    "magit/magit-branch.el"             ; requires: magit, magit-reset
    "magit/magit-bisect.el"
    "magit/magit-blame.el"
    "magit/magit-refs.el"
    "magit/magit-files.el"
    "magit/magit-ediff.el"
    "magit/magit-extras.el"
    "magit/magit-bookmark.el"
    "magit/magit-gitignore.el"
    "magit/magit-sparse-checkout.el"
    "magit/magit-bundle.el"
    "magit/magit-dired.el"
    "magit/git-rebase.el"               ; requires: magit, with-editor

    ;; ================================================================
    ;; dape  (requires project + many built-ins)
    ;; ================================================================

    "my-tools/dape.el"

    ;; ================================================================
    ;; Custom davidc libraries
    ;; ================================================================

    "davidc.el"
    "davidc-find.el"                    ; requires: dired, find-dired, grep, thingatpt
    "davidc-format.el"
    "davidc-region.el"                  ; requires: cl-lib
    "davidc-rename-symbol.el"
    "davidc-restart.el"
    "davidc-symbol-highlight.el"
    "davidc-hideshow.el"
    "davidc-flymake.el"                 ; requires: flymake, cl-lib
    "davidc-markdown-mode.el"           ; requires: font-lock
    "davidc-harpoon.el"                 ; requires: bookmark
    "davidc-ido.el"                     ; requires: ido
    "davidc-skeletons.el"               ; requires: skeleton
    "davidc-org-config.el"              ; requires: org, org-agenda, org-capture
    "davidc-yaml-mode.el"               ; requires: font-lock
    "davidc-outline.el"                 ; requires: outline, rx
    "davidc-quick-docs.el"              ; requires: eldoc, flymake
    "davidc-dape.el"                    ; requires: dape
    )
  "Files to byte-compile, listed in dependency order.")

;; ---------------------------------------------------------------------------
;; Build runner

(defvar davidc-build--errors 0)
(defvar davidc-build--skipped 0)

(defun davidc-build--compile (rel-path)
  "Byte-compile REL-PATH (relative to `davidc-build--lisp-dir').
Increments `davidc-build--errors' on failure, `davidc-build--skipped' when missing."
  (let ((src (expand-file-name rel-path davidc-build--lisp-dir)))
    (cond
     ((not (file-exists-p src))
      (message "  SKIP  (not found) %s" rel-path)
      (cl-incf davidc-build--skipped))
     (t
      (message "  compile  %s" rel-path)
      (unless (byte-compile-file src)
        (cl-incf davidc-build--errors))))))

;; Compiler settings
(setq byte-compile-verbose nil
      byte-compile-warnings t)

(message "")
(message "----------------------------------------")
(message "|  dot-emacs byte-compilation          |")
(message "----------------------------------------")
(message "Lisp root : %s" davidc-build--lisp-dir)
(message "Files     : %d" (length davidc-build--files))
(message "")

(dolist (f davidc-build--files)
  (davidc-build--compile f))

(let ((compiled (- (length davidc-build--files) davidc-build--skipped davidc-build--errors)))
  (message "")
  (message "---------------------------------------")
  (message "  Compiled : %d" compiled)
  (message "  Skipped  : %d" davidc-build--skipped)
  (message "  Errors   : %d" davidc-build--errors)
  (message "---------------------------------------")
  (message ""))

(when (> davidc-build--errors 0)
  (kill-emacs 1))

;;; build.el ends here
