;;; -*- lexical-binding: t -*-
;;
;; Sailfish Template Mode for Rust HTML templates.
;;
;; This mode is derived from `mhtml-mode' to provide syntax highlighting
;; for Sailfish Rust template files (.stpl).
;;

(define-derived-mode sailfish-template-mode mhtml-mode "Sailfish"
  "Major mode for Sailfish Rust template files.
Derived from `mhtml-mode' for HTML syntax highlighting.")

(provide 'davidc-sailfish)
