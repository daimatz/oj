(require :drakma)

(defclass OJ ()
     ((id :accessor id
          :initarg :id)
      (user :accessor user
            :initarg :user)
      (pass :accessor pass
            :initarg :pass)
      (ext-lang-map :accessor ext-lang-map
                    :initarg :ext-lang-map)
      (url :accessor url)
      (url-problem-id :accessor url-problem-id)
      (url-submit :accessor url-submit)
      (url-login :accessor url-login)
      (login-params :accessor login-params)
      (lang-field :accessor lang-field)
      ))

(defmethod get-sample ((c OJ))
  (princ (format nil "Get sample ~A ..." (id c)))
  (multiple-value-bind (body status header)
      (drakma:http-request (format nil "~A~A~A" (url c) (url-problem-id c) (id c)))
    (declare (ignore status header))
    ;(format t "~&Status: ~A~%Body: ~A~%Header: ~A~%" status body header)
    (with-open-file (stream (format nil "~A~A" (id c) sample-infile-ext)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (princ (sample-input c body) stream))
    (with-open-file (stream (format nil "~A~A" (id c) sample-outfile-ext)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (princ (sample-output c body) stream))))

(defmethod submit ((c OJ) file)
  (if (null file) (setq file (format nil "~A~A" (id c) default-program-ext)))
  (let ((ext (pathname-type file))
        (source (with-open-file (in file :if-does-not-exist :error)
                  (let ((str ""))
                    (loop for line = (read-line in nil)
                          while line do
                            (setq str (format nil "~A~A~%" str line)))
                    str)))
        (cookie-jar (make-instance 'drakma:cookie-jar)))
    (when (url-login c)
      (format t "Login ...~%")
      (drakma:http-request (format nil "~A~A" (url c) (url-login c))
       :method :post
       :parameters (login-params c)
       :cookie-jar cookie-jar))
    (format t "Submit as ~A code ...~%" (cdr (assoc ext (ext-lang-map c) :test 'string=)))
    (drakma:http-request (format nil "~A~A" (url c) (url-submit c))
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
