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

(load "setting.lisp")

(ql:quickload :drakma)
(ql:quickload :s-http-client)
(ql:quickload :cl-ppcre)

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
  (format t  "  <command>: Command. 'get' or 'submit'~%")
  (format t "  <number>:  Problem Number.~%"))


;;;; OJ base class
(defclass OJ ()
     ((id :accessor oj-id
          :initarg :id)
      (user :accessor oj-user
            :initarg :user)
      (pass :accessor oj-pass
            :initarg :pass)
      (ext-lang-map :accessor oj-ext-lang-map
                    :initarg :ext-lang-map)
      (url :accessor oj-url)
      (url-problem-id :accessor oj-url-problem-id)
      (url-submit :accessor oj-url-submit)
      (url-login :accessor oj-url-login)
      (login-params :accessor oj-login-params)
      (lang-field :accessor oj-lang-field)
      ))

(defgeneric get-sample (obj))
(defgeneric submit (obj file))
(defgeneric sample-input (obj html))
(defgeneric sample-output (obj html))
(defgeneric submit-params (obj source ext))

(defmethod get-sample ((c OJ))
  (princ (format nil "Get sample ~A ..." (oj-id c)))
  (multiple-value-bind (body status header uri kept)
      (s-http-client:do-http-request (format nil "~A~A~A" (oj-url c) (oj-url-problem-id c) (oj-id c)))
    (declare (ignore status header uri kept))
    ;(format t "~&Status: ~A~%Body: ~A~%Header: ~A~%" status body header)
    (with-open-file (stream (format nil "~A~A" (oj-id c) sample-infile-ext)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (princ (sample-input c body) stream))
    (with-open-file (stream (format nil "~A~A" (oj-id c) sample-outfile-ext)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (princ (sample-output c body) stream))))

(defmethod submit ((c OJ) file)
  (if (null file) (setq file (format nil "~A~A" (oj-id c) default-program-ext)))
  (let ((ext (pathname-type file))
        (source (with-open-file (in file :if-does-not-exist :error)
                  (let ((str ""))
                    (loop for line = (read-line in nil)
                          while line do
                            (setq str (format nil "~A~A~%" str line)))
                    str)))
        (cookie-jar (make-instance 'drakma:cookie-jar)))
    (when (oj-url-login c)
      (format t "Login ...~%")
      (drakma:http-request (format nil "~A~A" (oj-url c) (oj-url-login c))
       :method :post
       :parameters (oj-login-params c)
       :cookie-jar cookie-jar))
    (format t "Submit as ~A code ...~%" (cdr (assoc ext (oj-ext-lang-map c) :test 'string=)))
    (let ((lang (cdr (assoc
                      (cdr (assoc ext (oj-ext-lang-map c) :test 'string=))
                      (oj-lang-field c)
                      :test 'string=))))
      (drakma:http-request (format nil "~A~A" (oj-url c) (oj-url-submit c))
       :method :post
       :parameters (submit-params c source lang)
       :cookie-jar cookie-jar))))

(defmethod sample-input ((c OJ) html)
  (declare (ignore html))
  "sample-input")

(defmethod sample-output ((c OJ) html)
  (declare (ignore html))
  "sample-output")

(defmethod submit-params ((c OJ) source ext)
  (declare (ignore source ext))
  "submit-params")


;;;; POJ
(defclass POJ (OJ) ())

(defmethod initialize-instance :after ((c POJ) &rest initargs)
  (declare (ignore initargs))
  (setf (oj-url c) "http://poj.org/")
  (setf (oj-url-problem-id c) "problem?id=")
  (setf (oj-url-login c) "login")
  (setf (oj-url-submit c) "submit")
  (setf (oj-login-params c) `(("user_id1" . ,(oj-user c))
                              ("password1" . ,(oj-pass c))))
  (setf (oj-lang-field c) '(("G++" . "0") ("GCC" . "1") ("Java" . "2") ("Pascal" . "3")
                            ("C++" . "4") ("C" . "5") ("Fortran" . "6")))
  )

(defmethod sample-input ((c POJ) html)
  (ppcre:register-groups-bind (in)
   ("<pre class=\"sio\">((.|\\r|\\n)+?)<\/pre>" html)
   (format nil "~A~%" in)))

(defmethod sample-output ((c POJ) html)
  (ppcre:register-groups-bind (nil nil nil out)
   ("<pre class=\"sio\">((.|\\r|\\n)+?)<\/pre>(.|\\r|\\n)+?<pre class=\"sio\">((.|\\r|\\n)+?)<\/pre>" html)
   (format nil "~A~%" out)))

(defmethod submit-params ((c POJ) source lang)
  `(("language" . ,lang)
    ("problem_id" . ,(format nil "~A" (oj-id c)))
    ("source" . ,source)
    ("submit" . "Submit")))


;;;; AOJ
(defclass AOJ (OJ) ())

(defmethod initialize-instance :after ((c AOJ) &rest initargs)
  (declare (ignore initargs))
  (setf (oj-url c) "http://judge.u-aizu.ac.jp/onlinejudge/")
  (setf (oj-url-problem-id c) "description.jsp?id=")
  (setf (oj-url-login c) nil)
  (setf (oj-url-submit c) "servlet/Submit")
  (setf (oj-lang-field c) '(("C" . "C") ("C++" . "C++") ("C++11" . "C++11")
                            ("JAVA" . "JAVA") ("C#" . "C#") ("D" . "D")))
  )

(defmethod sample-input ((c AOJ) html)
  (ppcre:register-groups-bind (nil in)
   ("<[hH][1-6]>Sample Input<\/[hH][1-6]>(.|\\r|\\n)+?<pre>[\\r\\n]+?((.|\\r|\\n)+?)[\\r\\n]+?<\/pre>" html)
   (format nil "~A~%" in)))

(defmethod sample-output ((c AOJ) html)
  (ppcre:register-groups-bind (nil out)
   ("<[hH][1-6]>Output for the Sample Input<\/[hH][1-6]>(.|\\r|\\n)+?<pre>[\\r\\n]+?((.|\\r|\\n)+?)[\\r\\n]+?<\/pre>" html)
   (format nil "~A~%" out)))

(defmethod submit-params ((c AOJ) source lang)
  `(("userID" . ,(oj-user c))
    ("password" . ,(oj-pass c))
    ("problemNO" . ,(format nil "~A" (oj-id c)))
    ("language" . ,lang)
    ("sourceCode" . ,source)
    ("submit" . "Send")))



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
        (t (print-usage)
           (ccl:quit)))
  (cond ((string= command "get")
         (get-sample oj))
        ((string= command "submit")
         (submit oj nil))
        (t (print-usage)
           (ccl:quit))))

(ccl:quit)
