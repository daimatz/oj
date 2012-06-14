(ql:quickload :cl-ppcre)
(load "base.lisp")

(defclass AOJ (OJ)
     ())

(defmethod initialize-instance :after ((c AOJ) &rest initargs)
  (declare (ignore initargs))
  (setf (url c) "http://judge.u-aizu.ac.jp/onlinejudge/")
  (setf (url-problem-id c) "description.jsp?id=")
  (setf (url-login c) nil)
  (setf (url-submit c) "servlet/Submit")
  (setf (lang-field c) '(("C" . "C") ("C++" . "C++") ("C++11" . "C++11")
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

(defmethod submit-params ((c AOJ) source ext)
  `(("userID" . ,(user c))
    ("password" . ,(pass c))
    ("problemNO" . ,(format nil "~A" (id c)))
    ("language" . ,(cdr (assoc
                         (cdr (assoc ext (ext-lang-map c) :test 'string=))
                         (lang-field c)
                         :test 'string=)))
    ("sourceCode" . ,source)
    ("submit" . "Send")))
