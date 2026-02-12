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

;; (define-skeleton davidc-skeleton-rust-result
;;   "Insert a Rust function returning Result."
;;   "Function name: "
;;   "fn " str "() -> Result<" (skeleton-read "Ok type: ") ", " (skeleton-read "Err type: ") "> {\n"
;;   "    " _ "\n"
;;   "}" > \n)

;; (define-skeleton davidc-skeleton-rust-option
;;   "Insert a Rust function returning Option."
;;   "Function name: "
;;   "fn " str "() -> Option<" (skeleton-read "Type: ") "> {\n"
;;   "    " _ "\n"
;;   "}" > \n)

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
  > "match " _ " {" \n
  > "    " \n
  > "}" \n)

;; TODO: Write a "match" for handling an Option.

(define-skeleton davidc-skeleton-rust-if-let
  "Insert a Rust if let statement."
  "Outer variable: "
  > "if let Some(" (skeleton-read "Inner variable: ") ") = " str " {" \n
  > "    " _ \n
  > "}" \n)

(define-skeleton davidc-skeleton-rust-for
  "Insert a Rust for loop."
  "Iteration variable: "
  > "for " str " in " (skeleton-read "Iterable: ") " {" \n
  > "    " _ \n
  > "}" \n)

(define-skeleton davidc-skeleton-rust-while
  "Insert a Rust while loop."
  "While condition: "
  > "while " str " {" \n
  > "    " _ \n
  > "}" \n)

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

(define-skeleton davidc-skeleton-python-function
  "Insert a Python function definition."
  "Function name: "
  "def " str "():\n"
  "    " _ "\n" > \n)

(define-skeleton davidc-skeleton-python-async-function
  "Insert a Python async function definition."
  "Function name: "
  "async def " str "():\n"
  "    " _ "\n" > \n)

(define-skeleton davidc-skeleton-python-dataclass
  "Insert a Python dataclass definition."
  "Class name: "
  "from dataclasses import dataclass\n\n"
  "@dataclass\n"
  "class " str ":\n"
  "    " _ "\n" > \n)

(define-skeleton davidc-skeleton-python-class
  "Insert a Python class definition."
  "Class Name: "
  "class " str ":\n"
  "    def __init__(self):\n"
  "        " _ "\n" > \n)

(define-skeleton davidc-skeleton-python-method
  "Insert a Python method definition."
  "Method name: "
  "def " str "(self):\n"
  "    " _ "\n" > \n)

(define-skeleton davidc-skeleton-python-docstring
  "Insert a Python docstring."
  "Docstring: "
  "\"\"\"" str "\"\"\"\n" \n)

(define-skeleton davidc-skeleton-python-for
  "Insert a Python for loop."
  "Loop variable: "
  "for " str " in " (skeleton-read "Iterable: ") ":" \n
  > "    " _ \n)

(define-skeleton davidc-skeleton-python-while
  "Insert a Python while loop."
  "While condition: "
  "while " str ":" \n
  > "    " _ \n)

(define-skeleton davidc-skeleton-python-if
  "Insert a Python if statement."
  "If condition: "
  "if " str ":" \n
  > "    " _ \n)

(define-skeleton davidc-skeleton-python-if-else
  "Insert a Python if-else statement."
  "If condition: "
  "if " str ":" \n
  > "    " _ \n
  "else:" \n
  > "    pass" \n)

(define-skeleton davidc-skeleton-python-try-except
  "Insert a Python try-except block."
  "Exception type: "
  "try:" \n
  > "    " _ \n
  "except " str " as e:" \n
  > "    raise" \n)

(define-skeleton davidc-skeleton-python-with
  "Insert a Python with statement."
  "Context object: "
  "with " str " as name:" \n
  > "    " _ \n)

(define-skeleton davidc-skeleton-python-main
  "Insert a Python if __name__ == '__main__' block."
  nil
  > "if __name__ == \"__main__\":\n"
  "    " _ "\n" > \n)

;;; ------------------------------------------------------------------
;;; SMART SELECTION
;;; ------------------------------------------------------------------

(defvar davidc-skeleton-registry
  '((rust-mode . (("main"      . davidc-skeleton-rust-main)
                  ("test"      . davidc-skeleton-rust-test)
                  ("fn"        . davidc-skeleton-rust-fn)
                  ("fn-return" . davidc-skeleton-rust-fn-return)
                  ("struct"    . davidc-skeleton-rust-struct)
                  ("enum"      . davidc-skeleton-rust-enum)
                  ("impl"      . davidc-skeleton-rust-impl)
                  ("match"     . davidc-skeleton-rust-match)
                  ("if-let"    . davidc-skeleton-rust-if-let)
                  ("for"       . davidc-skeleton-rust-for)
                  ("while"     . davidc-skeleton-rust-while)))
    (c-mode    . (("main"          . davidc-skeleton-c-main)
                  ("include-guard" . davidc-skeleton-c-include-guard)
                  ("include"       . davidc-skeleton-c-include)
                  ("comment"       . davidc-skeleton-c-comment)
                  ("printf"        . davidc-skeleton-c-printf)
                  ("printf-newline" . davidc-skeleton-c-printf-newline)
                  ("printf-flush"  . davidc-skeleton-c-printf-flush)
                  ("for"           . davidc-skeleton-c-loop-for)
                  ("while"         . davidc-skeleton-c-loop-while)))
    (c++-mode  . (("main"          . davidc-skeleton-cc-main)
                  ("include"       . davidc-skeleton-c-include)
                  ("include-guard" . davidc-skeleton-c-include-guard)
                  ("comment"       . davidc-skeleton-c-comment)
                  ("for"           . davidc-skeleton-c-loop-for)
                  ("while"         . davidc-skeleton-c-loop-while)
                  ("endl"          . davidc-skeleton-cc-endl)))
    (python-mode . (("script"         . davidc-skeleton-python-script)
                    ("class"          . davidc-skeleton-python-class)
                    ("function"       . davidc-skeleton-python-function)
                    ("docstring"      . davidc-skeleton-python-docstring)
                    ("method"         . davidc-skeleton-python-method)
                    ("for"            . davidc-skeleton-python-for)
                    ("while"          . davidc-skeleton-python-while)
                    ("if"             . davidc-skeleton-python-if)
                    ("if-else"        . davidc-skeleton-python-if-else)
                    ("try-except"     . davidc-skeleton-python-try-except)
                    ("with"           . davidc-skeleton-python-with)
                    ("main"           . davidc-skeleton-python-main)
                    ("dataclass"      . davidc-skeleton-python-dataclass)
                    ("async-function" . davidc-skeleton-python-async-function))))
  "Alist mapping major modes to an alist of (SHORT-NAME . SKELETON-FUNCTION).")

(defun davidc-skeleton--get-relevant-definitions ()
  "Return an alist of (SHORT-NAME . SKELETON-FUNCTION) relevant to the current major mode.
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
         (names (mapcar #'car candidates))
         (choice (if candidates
                     (completing-read "Insert skeleton: " names nil t)
                   nil)))
    (if choice
        (funcall (cdr (assoc choice candidates)))
      (message "No skeletons defined for %s" major-mode))))

(provide 'davidc-skeletons)
;;; davidc-skeletons.el ends here
