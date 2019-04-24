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

(defclass parser ()
  ((input-stream :initarg :input-stream)
   (state :initform :parsing-first-line)
   (parsed-request :initform (make-instance 'http-request)
                   :reader parsed-request)))

(defun parse-method-and-target (line)
  (values "GET" "/home"))

(defun parse-header-line (line)
  (make-instance 'http-header
                 :name "content length"
                 :value "1234"))

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

(defmethod parse ((parser parser))
  (with-slots (input-stream state buffer parsed-request) parser
    (ecase state
      (:parsing-first-line
       (multiple-value-bind (request-method target)
           (parse-method-and-target (read-line-with-crlf input-stream))
         (setf (target parsed-request) target)
         (setf (request-method parsed-request) request-method)
         (setf state :parsing-headers)))
      (:parsing-headers
       (let ((line (read-line-with-crlf input-stream)))
         (if (zerop (length line))
             (setf state :parsing-body)
             (with-slots (headers) parsed-request
               (setf headers (cons (parse-header-line line)
                                   headers)))))))))
