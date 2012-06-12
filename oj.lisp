(defvar sample-infile-ext ".in")
(defvar sample-outfile-ext ".out")
(defvar default-program-ext ".cpp")

(load "base.lisp")
(load "poj.lisp")
(load "aoj.lisp")

(defvar setting
    '((poj . ((user . "username")
              (pass . "password")
              (ext-lang-map . (("cpp" . "G++") ("cc" . "G++") ("c" . "GCC") ("java" . "Java")))))
      (aoj . ((user . "username")
              (pass . "password")
              (ext-lang-map . (("cpp" . "C++11") ("cc" . "C++11") ("c" . "C") ("java" . "JAVA")))))
      ))

(defun get-args ()
  #+allegro (system:command-line-arguments)
  #+sbcl (cdr sb-ext:*posix-argv*)
  #+clisp ext:*args*
  #+ecl (cddr (cddr (si:command-args)))
  #+cmu ext:*command-line-words*
  #+ccl (cdr (cdddr ccl:*command-line-argument-list*))
  #+lispworks system:*line-arguments-list*)

(defun setting (judge param)
  (cdr (assoc param (cdr (assoc judge setting)))))

(defun usage ()
  (format t "Usage: ./oj <judge> <command> <number>~%")
  (format t "  <judge>:   Online Judge System. poj, ...~%")
  (format t  "  <command>: Command. 'get' or 'submit'~%")
  (format t "  <number>:  Problem Number.~%"))

(let* ((oj nil)
       (args (get-args))
       (judge (nth 0 args))
       (command (nth 1 args))
       (number (nth 2 args)))
  (cond ((string= judge "poj")
         (setq oj (make-instance 'POJ
                     :id (parse-integer number)
                     :user (setting 'poj 'user)
                     :pass (setting 'poj 'pass)
                     :ext-lang-map (setting 'poj 'ext-lang-map))))
        ((string= judge "aoj")
         (setq oj (make-instance 'AOJ
                     :id (parse-integer number)
                     :user (setting 'aoj 'user)
                     :pass (setting 'aoj 'pass)
                     :ext-lang-map (setting 'aoj 'ext-lang-map))))
        (t (usage)
           (quit)))
  (cond ((string= command "get")
         (get-sample oj))
        ((string= command "submit")
         (submit oj nil))
        (t (usage)
           (quit))))

(quit)
