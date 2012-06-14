(ql:quickload :drakma)
(ql:quickload :s-http-client)

(defclass oj ()
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
    (drakma:http-request (format nil "~A~A" (oj-url c) (oj-url-submit c))
     :method :post
     :parameters (submit-params c source ext)
     :cookie-jar cookie-jar)))

(defmethod sample-input ((c OJ) html)
  (declare (ignore html))
  "sample-input")

(defmethod sample-output ((c OJ) html)
  (declare (ignore html))
  "sample-output")

(defmethod submit-params ((c OJ) source ext)
  (declare (ignore source ext))
  "submit-params")
