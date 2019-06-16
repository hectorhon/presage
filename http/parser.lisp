(in-package :com.hon.http)

(defclass http-request ()
  ((request-method :type string
                   :accessor request-method
                   :documentation "The \"method\" symbol is reserved")
   (target :type string
           :accessor target)
   (headers :initform nil
            :accessor headers
            :documentation "An alist of headers")
   (body :type string
         :accessor body)))

(defun get-header-value (header-name http-request)
  (assoc-value header-name (headers http-request) :test #'string-equal))

:parsing-first-line
:parsing-headers
:parsing-body

(defun read-line-with-crlf (input-stream)
  "Like read-line, but looks for \\r\\n as the line delimiter."
  (multiple-value-bind (line missing-newline-p) (read-line input-stream)
    (if missing-newline-p
        (values line missing-newline-p)
        (let ((last-character
               (elt line (1- (length line)))))
          (if (char= #\Return last-character)
              (values (subseq line 0 (- (length line) 1))
                      missing-newline-p)
              (multiple-value-bind (line-2 missing-newline-p-2)
                  (read-line-with-crlf input-stream)
                (values (concatenate 'string line line-2)
                        missing-newline-p-2)))))))

(check-equals "read-line-with-crlf"
              "abc defghi jkl"
              (with-input-from-string (s "abc defghi jkl
mno pqr")
                (read-line-with-crlf s)))

(defun parse-http-request (input-stream)
  (let ((state :parsing-first-line)
        (parsed-request (make-instance 'http-request)))
    (loop (ecase state
            (:parsing-first-line
             (let ((line (read-line-with-crlf input-stream)))
               (destructuring-bind (request-method* target* protocol*)
                   (com.hon.utils.string:split-string line " ")
                 (declare (ignore protocol*))
                 (with-slots (request-method target) parsed-request
                   (setf request-method request-method*)
                   (setf target target*)))
               (setf state :parsing-headers)))
            (:parsing-headers
             (let ((line (read-line-with-crlf input-stream)))
               (flet ((has-header (header-name)
                        (not (null (get-header-value header-name parsed-request)))))
                 (if (equal line "")
                     (cond ((has-header "Transfer-Encoding")
                            (error "Transfer-Encoding header not supported"))
                           ((has-header "Content-Length")
                            (setf state :parsing-body))
                           (t (return parsed-request)))
                     (destructuring-bind (header-name header-value)
                         (com.hon.utils.string:split-string line ":" 1)
                       (setf (headers parsed-request)
                             (acons (string-trim " " header-name)
                                    (string-trim " " header-value)
                                    (headers parsed-request))))))))
            (:parsing-body
             (setf (body parsed-request)
                   (loop
                      :with content-length
                        = (parse-integer (get-header-value "Content-Length" parsed-request))
                      :with body = (make-string content-length)
                      :for i :upfrom 0 :below content-length
                      :do (setf (char body i) (read-char input-stream))
                      :finally (return body)))
             (return parsed-request))))))

(check-equals "parse-http-request" T
              (with-input-from-string (input-stream "GET /home/users HTTP/1.1
User-Agent: Mozilla/4.0 (compatible; MSIE5.01; Windows NT)
Host: localhost:8080

")
                (let ((parsed-request (parse-http-request input-stream)))
                  (with-slots (request-method target headers) parsed-request
                    (and (equal "GET" request-method)
                         (equal "/home/users" target)
                         (equal 2 (length headers))
                         (equal "Mozilla/4.0 (compatible; MSIE5.01; Windows NT)"
                                (get-header-value "User-Agent" parsed-request)))))))

(check-equals "parse-http-request with body" T
              (with-input-from-string (input-stream "POST /home/users HTTP/1.1
User-Agent: Mozilla/4.0 (compatible; MSIE5.01; Windows NT)
Host: localhost:8080
Content-length: 14

some body here")
                (let ((parsed-request (parse-http-request input-stream)))
                  (with-slots (request-method target headers body) parsed-request
                    (and (equal "POST" request-method)
                         (equal "/home/users" target)
                         (equal 3 (length headers))
                         (equal "some body here" body))))))
