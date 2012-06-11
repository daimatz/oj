(require :cl-ppcre)
(load "base.lisp")

(defclass POJ (OJ)
     ())

(defmethod initialize-instance :after ((c POJ) &rest initargs)
  (declare (ignore initargs))
  (setf (url c) "http://poj.org/")
  (setf (url-problem-id c) "problem?id=")
  (setf (url-login c) "login")
  (setf (url-submit c) "submit")
  (setf (login-params c) `(("user_id1" . ,(user c))
                           ("password1" . ,(pass c))))
  (setf (lang-field c) '(("G++" . "0") ("GCC" . "1") ("Java" . "2") ("Pascal" . "3")
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

(defmethod submit-params ((c POJ) source ext)
  `(("language" . ,(cdr (assoc
                         (cdr (assoc ext (ext-lang-map c) :test 'string=))
                         (lang-field c)
                         :test 'string=)))
    ("problem_id" . ,(format nil "~A" (id c)))
    ("source" . ,source)
    ("submit" . "Submit")))
