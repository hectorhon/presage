(in-package :com.hon.http)

(defclass http-header ()
  ((name :type string
         :initarg :name)
   (value :type string
          :initarg :value)))

(defclass http-request ()
  ((request-method :type string
                   :accessor request-method
                   :documentation "The \"method\" symbol is reserved")
   (target :type string
           :accessor target)
   (headers :initform nil
            :accessor headers)))

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
                   (com.hon.string-utils:split-string line " ")
                 (declare (ignore protocol*))
                 (with-slots (request-method target) parsed-request
                   (setf request-method request-method*)
                   (setf target target*)))
               (setf state :parsing-headers)))
            (:parsing-headers
             (let ((line (read-line-with-crlf input-stream)))
               (destructuring-bind (before-first-colon &rest after-first-colon)
                   (com.hon.string-utils:split-string line ":")
                 (let* ((header-name before-first-colon)
                        (header-value (apply #'concatenate 'string after-first-colon))
                        (new-http-header (make-instance 'http-header
                                                        :name (string-trim " " header-name)
                                                        :value (string-trim " " header-value))))
                   (setf (headers parsed-request)
                         (cons new-http-header (headers parsed-request))))
                 (return parsed-request))))))))

(check-equals "parse-http-request" T
              (with-input-from-string (input-stream "GET /home/users HTTP/1.1
User-Agent: Mozilla/4.0 (compatible; MSIE5.01; Windows NT)

")
                (let ((parsed-request (parse-http-request input-stream)))
                  (with-slots (request-method target headers) parsed-request
                    (and (equal "GET" request-method)
                         (equal "/home/users" target)
                         (equal 1 (length headers))
                         (with-slots (name value) (first headers)
                           (and (equal "User-Agent" name)
                                (equal "Mozilla/4.0 (compatible; MSIE5.01; Windows NT)" value))))))))
