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

(define-skeleton davidc-skeleton-rust-fn
  "Insert a Rust function definition."
  "Function name: "
  "fn " str "() {\n"
  "    " _ "\n"
  "}" > \n)

(define-skeleton davidc-skeleton-rust-fn-return
  "Insert a Rust function with return type."
  "Function name: "
  "fn " str "() -> " (skeleton-read "Return type: ") " {\n"
  "    " _ "\n"
  "}" > \n)

(define-skeleton davidc-skeleton-rust-struct
  "Insert a Rust struct definition."
  "Struct name: "
  "struct " str " {\n"
  "    " _ "\n"
  "}" > \n)

(define-skeleton davidc-skeleton-rust-enum
  "Insert a Rust enum definition."
  "Enum name: "
  "enum " str " {\n"
  "    " _ "\n"
  "}" > \n)

(define-skeleton davidc-skeleton-rust-impl
  "Insert a Rust impl block."
  "Type name: "
  "impl " str " {\n"
  "    " _ "\n"
  "}" > \n)

(define-skeleton davidc-skeleton-rust-match
  "Insert a Rust match statement."
  nil
  > "match " _ " {\n"
  "    " "\n"
  "}" > \n)

(define-skeleton davidc-skeleton-rust-if-let
  "Insert a Rust if let statement."
  nil
  > "if let " _ " = " " {\n"
  "    " "\n"
  "}" > \n)

(define-skeleton davidc-skeleton-rust-for
  "Insert a Rust for loop."
  "Iterator variable: "
  > "for " str " in " _ " {\n"
  "    " "\n"
  "}" > \n)

(define-skeleton davidc-skeleton-rust-while
  "Insert a Rust while loop."
  nil
  > "while " _ " {\n"
  "    " "\n"
  "}" > \n)

(define-skeleton davidc-skeleton-rust-result
  "Insert a Rust function returning Result."
  "Function name: "
  "fn " str "() -> Result<" (skeleton-read "Ok type: ") ", " (skeleton-read "Err type: ") "> {\n"
  "    " _ "\n"
  "}" > \n)

(define-skeleton davidc-skeleton-rust-option
  "Insert a Rust function returning Option."
  "Function name: "
  "fn " str "() -> Option<" (skeleton-read "Type: ") "> {\n"
  "    " _ "\n"
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

(define-skeleton davidc-skeleton-python-function
  "Insert a Python function definition."
  "Function name: "
  "def " str "():\n"
  "    " _ "\n" > \n)

(define-skeleton davidc-skeleton-python-function-docstring
  "Insert a Python function with docstring."
  "Function name: "
  "def " str "():\n"
  "    \"\"\"" (skeleton-read "Docstring: ") "\"\"\"\n"
  "    " _ "\n" > \n)

(define-skeleton davidc-skeleton-python-method
  "Insert a Python method definition."
  "Method name: "
  "def " str "(self):\n"
  "    " _ "\n" > \n)

(define-skeleton davidc-skeleton-python-for
  "Insert a Python for loop."
  "Loop variable: "
  > "for " str " in " _ ":\n"
  "    " "\n" > \n)

(define-skeleton davidc-skeleton-python-while
  "Insert a Python while loop."
  nil
  > "while " _ ":\n"
  "    " "\n" > \n)

(define-skeleton davidc-skeleton-python-if
  "Insert a Python if statement."
  nil
  > "if " _ ":\n"
  "    " "\n" > \n)

(define-skeleton davidc-skeleton-python-if-else
  "Insert a Python if-else statement."
  nil
  > "if " _ ":\n"
  "    " "\n"
  > "else:\n"
  "    " "\n" > \n)

(define-skeleton davidc-skeleton-python-try-except
  "Insert a Python try-except block."
  "Exception type (or leave empty for generic): "
  > "try:\n"
  "    " _ "\n"
  > "except" (if (string= str "") "" (concat " " str)) ":\n"
  "    " "\n" > \n)

(define-skeleton davidc-skeleton-python-with
  "Insert a Python with statement."
  nil
  > "with " _ " as :\n"
  "    " "\n" > \n)

(define-skeleton davidc-skeleton-python-main
  "Insert a Python if __name__ == '__main__' block."
  nil
  > "if __name__ == \"__main__\":\n"
  "    " _ "\n" > \n)

(define-skeleton davidc-skeleton-python-dataclass
  "Insert a Python dataclass definition."
  "Class name: "
  "from dataclasses import dataclass\n\n"
  "@dataclass\n"
  "class " str ":\n"
  "    " _ "\n" > \n)

(define-skeleton davidc-skeleton-python-async-function
  "Insert a Python async function definition."
  "Function name: "
  "async def " str "():\n"
  "    " _ "\n" > \n)

;;; ------------------------------------------------------------------
;;; SMART SELECTION
;;; ------------------------------------------------------------------

(defvar davidc-skeleton-registry
  '((rust-mode . (davidc-skeleton-rust-main
                  davidc-skeleton-rust-test
                  davidc-skeleton-rust-fn
                  davidc-skeleton-rust-fn-return
                  davidc-skeleton-rust-struct
                  davidc-skeleton-rust-enum
                  davidc-skeleton-rust-impl
                  davidc-skeleton-rust-match
                  davidc-skeleton-rust-if-let
                  davidc-skeleton-rust-for
                  davidc-skeleton-rust-while
                  davidc-skeleton-rust-result
                  davidc-skeleton-rust-option))
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
                    davidc-skeleton-python-class
                    davidc-skeleton-python-function
                    davidc-skeleton-python-function-docstring
                    davidc-skeleton-python-method
                    davidc-skeleton-python-for
                    davidc-skeleton-python-while
                    davidc-skeleton-python-if
                    davidc-skeleton-python-if-else
                    davidc-skeleton-python-try-except
                    davidc-skeleton-python-with
                    davidc-skeleton-python-main
                    davidc-skeleton-python-dataclass
                    davidc-skeleton-python-async-function)))
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
