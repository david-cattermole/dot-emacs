;;; -*- lexical-binding: t -*-
;;; davidc-skeletons.el --- Custom skeleton snippets for programming modes.

(require 'skeleton)

;;; ------------------------------------------------------------------
;;; RUST SKELETONS
;;; ------------------------------------------------------------------

(define-skeleton davidc-skeleton-rust-main
  "Insert a basic Rust main function structure."
  nil ;; No prompt needed
  "fn main() {\n"
  "    " _ "\n"
  "}" > \n)

(define-skeleton davidc-skeleton-rust-test
  "Insert a Rust test module."
  nil
  "#[cfg(test)]\n"
  "mod tests {\n"
  "    use super::*;\n\n"
  "    #[test]\n"
  "    fn it_works() {\n"
  "        " _ "\n"
  "    }\n"
  "}" > \n)

;;; ------------------------------------------------------------------
;;; C / C++ SKELETONS
;;; ------------------------------------------------------------------
;;
;; Some of these skeletons come from Stefan Kamphausen (www.skamphausen.de):
;; https://github.com/ska2342/ska-init-files/blob/master/dot.emacs.d/init.el
;; 

(define-skeleton davidc-skeleton-c-main
  "Insert a standard C main function with includes."
  nil
  "#include <stdio.h>\n"
  "#include <stdlib.h>\n\n"
  "int main(int argc, char *argv[]) {\n"
  "    " _ "\n"
  "    return 0;\n"
  "}" > \n)

(define-skeleton davidc-skeleton-cc-main
  "Insert a standard C++ main function with iostream."
  nil
  "#include <iostream>\n\n"
  "int main(int argc, char *argv[]) {\n"
  "    " _ "\n"
  "    return 0;\n"
  "}" > \n)


(define-skeleton davidc-skeleton-c-include
  "Insert a precompiler include statement, asking for what to include.       
You need to give the quotation marks or the angles yourself."
  "include what? "
  > "#include " str
  )

(define-skeleton davidc-skeleton-c-include-guard
  "Insert a header guard for C/C++ header files."
  "Name of the guard (e.g., MY_HEADER_H): "
  (setq v1 (upcase str)) ;; Convert input to uppercase
  "#ifndef " v1 \n
  "#define " v1 \n
  \n
  _ \n
  \n
  "#endif // " v1 > \n)

;; From Stefan Kamphausen (www.skamphausen.de).
(define-skeleton davidc-skeleton-c-comment
  "Insert comment."
  nil
  "/* "
  > _
  " */"
  )

;; From Stefan Kamphausen (www.skamphausen.de).
(define-skeleton davidc-skeleton-c-printf
  "Insert the common printf statement at point."
  nil
  > "printf(\""
  _
  "\");"
  )

;; From Stefan Kamphausen (www.skamphausen.de).
(define-skeleton davidc-skeleton-c-printf-flush
  "Insert the common printf statement followed by an fflush at point."
  nil
  > "printf(\""
  _
  "\");fflush(stdout);"
  )

;; From Stefan Kamphausen (www.skamphausen.de).
(define-skeleton davidc-skeleton-c-printf-newline
  "Insert a printf statement with newline"
  nil
  "printf(\"\\n\");"
)

;; From Stefan Kamphausen (www.skamphausen.de).
(define-skeleton davidc-skeleton-c-loop-for
  "Insert a for-loop with an int counter variable."
  "Counter variable(int): "
  >"for(int " str "=0;" str "<" _ ";" str "++) {" \n
  \n
  > "}" \n
  )

;; From Stefan Kamphausen (www.skamphausen.de).
(define-skeleton davidc-skeleton-c-loop-while
  "Insert a while-loop template."
  > "while(" _ ") {"\n
  \n
  >"}" \n
  )

;; C++ Special.
;;
;; From Stefan Kamphausen (www.skamphausen.de).
(defun davidc-skeleton-cc-endl ()
  "Insert the correct endl-string."
  (interactive)
  (when (not (save-excursion
               (skip-chars-backward " \t\n<")
               (looking-at "\\s-*<")))
    (just-one-space)
    (insert "<<"))
    (just-one-space)
  (insert "endl;\n"))


;;; ------------------------------------------------------------------
;;; PYTHON SKELETONS
;;; ------------------------------------------------------------------

(define-skeleton davidc-skeleton-python-script
  "Insert a Python script structure with main check."
  "Description: "
  "#!/usr/bin/env python3\n"
  "\"\"\"\n"
  str | "Script description here." \n
  "\"\"\"\n\n"
  "import sys\n\n"
  "def main():\n"
  "    " _ "\n\n"
  "if __name__ == \"__main__\":\n"
  "    main()" > \n)

(define-skeleton davidc-skeleton-python-class
  "Insert a Python class definition."
  "Class Name: "
  "class " str ":\n"
  "    def __init__(self):\n"
  "        " _ "\n" > \n)

;;; ------------------------------------------------------------------
;;; SMART SELECTION
;;; ------------------------------------------------------------------

(defvar davidc-skeleton-registry
  '((rust-mode . (davidc-skeleton-rust-main
                  davidc-skeleton-rust-test))
    (c-mode    . (davidc-skeleton-c-main
                  davidc-skeleton-c-include-guard
                  davidc-skeleton-c-include
                  davidc-skeleton-c-comment
                  davidc-skeleton-c-printf
                  davidc-skeleton-c-printf-newline
                  davidc-skeleton-c-printf-flush
                  davidc-skeleton-c-loop-for
                  davidc-skeleton-c-loop-while))
    (c++-mode  . (davidc-skeleton-cc-main
                  davidc-skeleton-c-include
                  davidc-skeleton-c-include-guard
                  davidc-skeleton-c-comment
                  davidc-skeleton-c-loop-for
                  davidc-skeleton-c-loop-while
                  davidc-skeleton-cc-endl))
    (python-mode . (davidc-skeleton-python-script
                    davidc-skeleton-python-class)))
  "Alist mapping major modes to a list of skeleton command symbols.")

(defun davidc-skeleton--get-relevant-definitions ()
  "Return a list of skeleton symbols relevant to the current major mode.
Checks derived modes so that 'c++-ts-mode' inherits 'c++-mode' skeletons."
  (let ((candidates '()))
    (dolist (entry davidc-skeleton-registry)
      (let ((mode (car entry))
            (skeletons (cdr entry)))
        (when (or (eq major-mode mode)
                  (derived-mode-p mode))
          (setq candidates (append candidates skeletons)))))
    (delete-dups candidates)))

;;;###autoload
(defun davidc-skeleton-insert ()
  "Prompt user with completing-read to insert a skeleton relevant to the current mode."
  (interactive)
  (let* ((candidates (davidc-skeleton--get-relevant-definitions))
         ;; Convert symbols to strings for completing-read
         (candidate-strings (mapcar #'symbol-name candidates))
         (choice (if candidates
                     (completing-read "Insert skeleton: " candidate-strings nil t)
                   nil)))
    (if choice
        (funcall (intern choice))
      (message "No skeletons defined for %s" major-mode))))

(provide 'davidc-skeletons)
;;; davidc-skeletons.el ends here
