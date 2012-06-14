(in-package :contextl-user)

(defvar sample-infile-ext ".in")
(defvar sample-outfile-ext ".out")
(defvar default-program-ext ".cpp")

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

(defun print-usage ()
  (format t "Usage: ./oj <judge> <command> <number>~%")
  (format t "  <judge>:   Online Judge System. poj, ...~%")
  (format t "  <command>: Command. 'get' or 'submit'~%")
  (format t "  <number>:  Problem Number.~%"))

(ql:quickload :contextl)
(ql:quickload :drakma)
(ql:quickload :s-http-client)
(ql:quickload :cl-ppcre)

(deflayer cookie-layer)

(define-layered-class OJ ()
  ((id :layered-accessor oj-id
       :initarg :id)
   (ext-lang-map :layered-accessor oj-ext-lang-map
                 :initarg :ext-lang-map)
   (sample-input-func :layered-accessor oj-sample-input-func
                      :initarg :sample-input-func)
   (sample-output-func :layered-accessor oj-sample-output-func
                       :initarg :sample-output-func)
   (submit-params-func :layered-accessor oj-submit-params-func
                       :initarg :submit-params-func)
   (url :layered-accessor oj-url
        :initarg :url)
   (url-problem-id :layered-accessor oj-url-problem-id
                   :initarg :url-problem-id)
   (url-submit :layered-accessor oj-url-submit
               :initarg :url-submit)
   (lang-field :layered-accessor oj-lang-field
               :initarg :lang-field)
   (cookie :layered-accessor oj-cookie
           :initform nil)
   ))

(define-layered-class OJ
  :in-layer cookie-layer ()
  ((url-login :layered-accessor oj-url-login
              :initarg :url-login)
   (login-params :layered-accessor oj-login-params
                 :initarg :login-params)))

(define-layered-function get-sample (obj))

(define-layered-method get-sample ((c OJ))
  (princ (format nil "Get sample ~A ..." (oj-id c)))
  (multiple-value-bind (body status header uri kept)
      (s-http-client:do-http-request (format nil "~A~A~A" (oj-url c) (oj-url-problem-id c) (oj-id c)))
    (declare (ignore status header uri kept))
    (with-open-file (stream (format nil "~A~A" (oj-id c) sample-infile-ext)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (princ (funcall (oj-sample-input-func c) body) stream))
    (with-open-file (stream (format nil "~A~A" (oj-id c) sample-outfile-ext)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (princ (funcall (oj-sample-output-func c) body) stream))))

(define-layered-function submit (obj file))

(define-layered-method submit ((c OJ) file)
  (if (null file) (setq file (format nil "~A~A" (oj-id c) default-program-ext)))
  (let ((ext (pathname-type file))
        (source (with-open-file (in file :if-does-not-exist :error)
                  (let ((str ""))
                    (loop for line = (read-line in nil)
                          while line do
                            (setq str (format nil "~A~A~%" str line)))
                    str))))
    (format t "Submit as ~A code ...~%" (cdr (assoc ext (oj-ext-lang-map c) :test 'string=)))
    (drakma:http-request (format nil "~A~A" (oj-url c) (oj-url-submit c))
     :method :post
     :parameters (funcall (oj-submit-params-func c) source ext)
     :cookie-jar (oj-cookie c))))

(define-layered-method submit
    :in cookie-layer ((c OJ) file)
    (let ((cookie-jar (make-instance 'drakma:cookie-jar)))
      (format t "Login ...~%")
      (drakma:http-request (format nil "~A~A" (oj-url c) (oj-url-login c))
       :method :post
       :parameters (oj-login-params c)
       :cookie-jar cookie-jar)
      (setf (oj-cookie c) cookie-jar)
      (call-next-method)))

(let* ((oj nil)
       (args (get-args))
       (judge (nth 0 args))
       (command (nth 1 args))
       (number (nth 2 args)))
  (cond ((string= judge "poj")
         (with-active-layers (cookie-layer)
           (setq oj (make-instance 'OJ
                       :id number
                       :ext-lang-map (setting 'poj 'ext-lang-map)
                       :sample-input-func #'(lambda (html)
                                              (ppcre:register-groups-bind (in)
                                               ("<pre class=\"sio\">((.|\\r|\\n)+?)<\/pre>" html)
                                               (format nil "~A~%" in)))
                       :sample-output-func #'(lambda (html)
                                               (ppcre:register-groups-bind (nil nil nil out)
                                                ("<pre class=\"sio\">((.|\\r|\\n)+?)<\/pre>(.|\\r|\\n)+?<pre class=\"sio\">((.|\\r|\\n)+?)<\/pre>" html)
                                                (format nil "~A~%" out)))
                       :submit-params-func #'(lambda (source ext)
                                               `(("language" . ,(cdr (assoc
                                                                      (cdr (assoc ext (oj-ext-lang-map c) :test 'string=))
                                                                      (oj-lang-field c)
                                                                      :test 'string=)))
                                                 ("problem_id" . ,(format nil "~A" (oj-id c)))
                                                 ("source" . ,source)
                                                 ("submit" . "Submit")))
                       :url "http://poj.org/"
                       :url-problem-id "problem?id="
                       :url-login "login"
                       :url-submit "submit"
                       :login-params `(("user_id1" . ,(setting 'poj 'user))
                                       ("password1" . ,(setting 'poj 'pass)))
                       :lang-field '(("G++" . "0") ("GCC" . "1") ("Java" . "2") ("Pascal" . "3")
                                     ("C++" . "4") ("C" . "5") ("Fortran" . "6"))))))
        ((string= judge "aoj")
         (setq oj (make-instance 'OJ
                     :id number
                     :ext-lang-map (setting 'aoj 'ext-lang-map)
                     :sample-input-func #'(lambda (html)
                                            (ppcre:register-groups-bind (nil in)
                                             ("<[hH][1-6]>Sample Input<\/[hH][1-6]>(.|\\r|\\n)+?<pre>[\\r\\n]+?((.|\\r|\\n)+?)[\\r\\n]+?<\/pre>" html)
                                             (format nil "~A~%" in)))
                     :sample-output-func #'(lambda (html)
                                             (ppcre:register-groups-bind (nil out)
                                              ("<[hH][1-6]>Output for the Sample Input<\/[hH][1-6]>(.|\\r|\\n)+?<pre>[\\r\\n]+?((.|\\r|\\n)+?)[\\r\\n]+?<\/pre>" html)
                                              (format nil "~A~%" out)))
                     :submit-params-func #'(lambda (source ext)
                                             `(("userID" . ,(setting 'aoj 'user))
                                               ("password" . ,(setting 'aoj 'pass))
                                               ("problemNO" . ,number)
                                               ("language" . ,(cdr (assoc
                                                                    (cdr (assoc ext (oj-ext-lang-map c) :test 'string=))
                                                                    (oj-lang-field c)
                                                                    :test 'string=)))
                                               ("sourceCode" . ,source)
                                               ("submit" . "Send")))
                     :url "http://judge.u-aizu.ac.jp/onlinejudge/"
                     :url-problem-id "description.jsp?id="
                     :url-submit "servlet/Submit"
                     :lang-field '(("C" . "C") ("C++" . "C++") ("C++11" . "C++11")
                                   ("JAVA" . "JAVA") ("C#" . "C#") ("D" . "D")))))
        (t (print-usage)
           (ccl:quit)))
  (cond ((string= command "get")
         (get-sample oj))
        ((string= command "submit")
         (submit oj nil))
        (t (print-usage)
           (ccl:quit))))

(ccl:quit)
