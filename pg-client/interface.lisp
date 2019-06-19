(in-package :com.hon.clients.postgresql)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-md5))

(defclass connection ()
  ((process-id :type integer)
   (secret-key :type integer)
   (backend-parameters :type list :initform nil)))

(defun connect (user database &key password gss-or-sspi-handler)
  (let ((connection (make-instance 'connection)))
    (send-message (make-instance 'startup-message :user user :database database))
    (loop :for response = (parse-startup-phase-1-response)
       :until (eq 'authentication-ok-message (type-of response))
       :do (ecase (type-of response)
             (error-response-message
              (error "The connection attempt has been rejected by the server."))
             (authentication-cleartext-password-message
              (send-message (make-instance 'password-message :password password)))
             (authentication-md5-password-message
              (labels ((byte-to-hex-string (byte)
                         (format nil "~(~2,'0X~)" byte))
                       (bytes-to-hex-string (bytes)
                         (apply #'concatenate 'string (map 'list #'byte-to-hex-string bytes)))
                       (string-to-bytes (str)
                         (map '(vector (unsigned-byte 8)) #'char-code str)))
                (let* ((salt (slot-value response 'salt))
                       (md5-1 (bytes-to-hex-string (sb-md5:md5sum-string (concatenate 'string password user))))
                       (md5-2 (bytes-to-hex-string (sb-md5:md5sum-sequence
                                                    (concatenate '(vector (unsigned-byte 8)) (string-to-bytes md5-1) salt))))
                       (password (concatenate 'string "md5" md5-2)))
                  (send-message (make-instance 'password-message :password password)))))
             (authentication-gss-message
              (send-message (make-instance 'password-message :password (funcall gss-or-sspi-handler nil))))
             (authentication-sspi-message
              (send-message (make-instance 'password-message :password (funcall gss-or-sspi-handler nil))))
             (authentication-gss-continue-message
              (let ((more-data (funcall gss-or-sspi-handler (slot-value response 'authentication-data))))
                (if more-data
                    (send-message (make-instance 'password-message :password more-data)))))
             (negotiate-protocol-version-message
              (#| this message is ignored |#))))
    (loop :for further-response = (parse-startup-phase-2-response)
       :until (eq 'ready-for-query-message (type-of further-response))
       :finally (return connection)
       :do (ecase (type-of further-response)
             (backend-key-data-message
              (with-slots (process-id secret-key) further-response
                (setf (slot-value connection 'process-id) process-id)
                (setf (slot-value connection 'secret-key) secret-key)))
             (parameter-status-message
              (with-slots (parameter-name parameter-value) further-response
                (push (cons parameter-name parameter-value)
                      (slot-value connection 'backend-parameters))))
             (error-response-message
              (error "Connection start-up failed."))
             (notice-response-message
              (with-slots (notice-fields) further-response
                (let ((severity) (message))
                  (loop :for notice-field :in notice-fields
                     :do (with-slots (field-type-byte field-value) notice-field
                           (cond ((eql field-type-byte (char-code #\S))
                                  (setf severity field-value))
                                 ((eql field-type-byte (char-code #\M))
                                  (setf message field-value)))))
                  (log-debug "~a ~a~%" severity message))))))))

(defun convert-data-row (row-description-message data-row-message)
  "Given a row description message, convert the received data row message into a list of
   field values."
  ;; Can probably be improved by creating a row reader from the row description message
  (loop :for (field-description column-value)
     :in (mapcar #'list
                 (field-descriptions row-description-message)
                 (slot-value data-row-message 'column-values))
     :collect (with-slots (data-type-oid format-code) field-description
                (with-slots (column-value-value) column-value
                  (if (eql 0 format-code)
                      (let ((value (concatenate 'string (map 'vector #'code-char column-value-value))))
                        (ecase data-type-oid
                          (705          ; unknown data type, treat as string
                           value)
                          (1043         ; varchar
                           value)
                          (23           ; int4
                           (parse-integer value))))
                      (error "Binary format not yet implemented"))))))

(defun simple-query (query-string)
  "Execute the query on *pg-stream* and return a list of lists. The first item in the
   list is the list of column names, the subsequent items are data rows."
  (send-message (make-instance 'query-message :query-string query-string))
  (loop :for response = (parse-simple-query-response)
     :with row-description-message = nil ; to be filled from response
     :and rows = nil                     ; to be filled from response
     :until (eq 'ready-for-query-message (type-of response))
     :finally (return (cons (mapcar #'field-name (field-descriptions row-description-message)) rows))
     :do (ecase (type-of response)
           (command-complete-message
            ())
           (copy-in-response
            (error "Not yet implemented"))
           (copy-out-response
            (error "Not yet implemented"))
           (row-description-message
            (setf row-description-message response))
           (data-row-message
            (push (convert-data-row row-description-message response) rows))
           (empty-query-response-message
            (return rows))
           (error-response-message
            (error "Error: ~a" response))
           (notice-response-message
            (log-debug "Notice: ~a" response)))))

(defgeneric to-pg-parameter-value (value)
  (:documentation "Convert value to postgres binary parameter format."))

(defmethod to-pg-parameter-value ((value null))
  (make-instance 'parameter-value
                 :parameter-value-length -1
                 :parameter-value nil))

(defmethod to-pg-parameter-value ((integer integer))
  (make-instance 'parameter-value
                 :parameter-value-length (/ 32 8)
                 :parameter-value (integer-to-bytes integer 32)))

(defmethod to-pg-parameter-value ((string string))
  (make-instance 'parameter-value
                 :parameter-value-length (length string)
                 :parameter-value (map 'vector #'char-code string)))

(defun query (query-string &rest parameters)
  "Perform a query with parameters."
  (send-message (make-instance 'parse-message
                               :destination-prepared-statement-name ""
                               :query-string query-string
                               :number-of-specified-parameter-data-types 0
                               :parameter-data-type-oids nil))
  (send-message (make-instance 'flush-message))
  (let ((response (parse-extended-query-parse-response)))
    (ecase (type-of response)
      (parse-complete-message nil)      ; good, continue
      (error-response-message (error "Error: ~a" response))))
  (send-message (make-instance 'bind-message
                               :destination-portal-name ""
                               :source-prepared-statement-name ""
                               :number-of-parameter-format-codes 1
                               :parameter-format-codes '(1)
                               :number-of-parameter-values (length parameters)
                               :parameter-values (mapcar #'to-pg-parameter-value parameters)
                               :number-of-result-column-format-codes 0
                               :result-column-format-codes nil))
  (send-message (make-instance 'flush-message))
  (let ((response (parse-extended-query-bind-response)))
    (ecase (type-of response)
      (bind-complete-message nil)       ; good, continue
      (error-response-message (error "Error: ~a" response))))
  (send-message (make-instance 'describe-message
                               :prepared-statement-or-portal (char-code #\P)
                               :prepared-statement-or-portal-name ""))
  (send-message (make-instance 'flush-message))
  (prog1 (let ((row-description-message (let ((response (parse-extended-query-describe-response)))
                                          (ecase (type-of response)
                                            (row-description-message response)
                                            (no-data-message nil)
                                            (error-response-message
                                             (error "Error: ~a" response))
                                            (notice-response-message
                                             (log-debug "Notice: ~a" response))))))
           (send-message (make-instance 'execute-message
                                        :portal-name ""
                                        :max-number-of-rows-to-return 0))
           (send-message (make-instance 'flush-message))
           (loop :for response = (parse-extended-query-execute-response)
              :with rows = nil                 ; to be filled from response
              :until (eq 'command-complete-message (type-of response))
              :finally (return (cons (mapcar #'field-name (field-descriptions row-description-message)) rows))
              :do (ecase (type-of response)
                    (copy-in-response
                     (error "Not yet implemented"))
                    (copy-out-response
                     (error "Not yet implemented"))
                    (data-row-message
                     (let ((x (convert-data-row row-description-message response)))
                       (push x rows)))
                    (empty-query-response-message
                     (return rows))
                    (error-response-message
                     (error "Error: ~a" response))
                    (notice-response-message
                     (log-debug "Notice: ~a" response))
                    (portal-suspended-message
                     (error "Not yet implemented")))))
    (send-message (make-instance 'sync-message))
    (parse-extended-query-sync-response)))

(defun test ()
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (unwind-protect
         (progn (sb-bsd-sockets:socket-connect socket "/var/run/postgresql/.s.PGSQL.5432")
                (let* ((pg-stream
                        (sb-bsd-sockets:socket-make-stream socket :input t :output t :timeout 5 :element-type :default))
                       (*standard-input* pg-stream)
                       (*standard-output* pg-stream))
                  (connect "presage" "presage" :password "presage")
                  (simple-query "select * from persons")
                  (query "select * from persons where age = $1 and name = $2;" 10 "james")))
      (sb-bsd-sockets:socket-shutdown socket :direction :io)
      (sb-bsd-sockets:socket-close socket)
      (print "socket closed"))))
