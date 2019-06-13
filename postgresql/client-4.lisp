(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-md5))

(defun log-debug (ctrl-string &rest args)
  (apply #'format t (concatenate 'string "DEBUG: " ctrl-string) args))

(defvar *pg-stream*)

;;;
;;; Write functions
;;;

(defun write-int* (bits value)
  (loop :for pos :downfrom (- bits 8) :to 0 :by 8
     :do (write-byte (ldb (byte 8 pos) value) *pg-stream*)))

(defun write-string* (str)
  (write-string str *pg-stream*)
  (write-byte 0 *pg-stream*))

(defun write-byte* (byte)
  (write-byte byte *pg-stream*))

(defun write-bytes* (bytes)
  (loop :for byte :across bytes
     :do (write-byte* byte)))

;;;
;;; Read functions
;;;

(defun read-byte* ()
  (let ((byte (read-byte *pg-stream*)))
    (log-debug "byte: ~a~%" byte)
    byte))

(defun read-bytes* (count)
  (loop :with arr = (make-array count :element-type 'unsigned-byte)
     :for i :from 0 :below count
     :do (setf (aref arr i) (read-byte*))
     :finally (return arr)))

(defun read-int* (bits)
  (let ((int (loop :for shift-count :downfrom (- bits 8) :to 0 :by 8
                :summing (ash (read-byte *pg-stream*) shift-count))))
    (log-debug "integer: ~a~%" int)
    int))

(defun read-ints* (bits count)
  (loop :with arr = (make-array count :element-type 'integer)
     :for i :from 0 :below count
     :do (setf (aref arr i) (read-int* bits))
     :finally (return arr)))

(defun read-string* ()
  (let ((str (coerce (loop :for c = (read-char *pg-stream*)
                        :if (eql #\null c) :return chars
                        :else :collecting c :into chars)
                     'string)))
    (log-debug "string: ~a~%" str)
    str))

;;;
;;; Message
;;;

(defclass message () ())

;;;
;;; Frontend messages
;;;

(defclass frontend-message (message) ())

(defclass startup-message (frontend-message)
  ((user     :type string :initarg :user)
   (database :type string :initarg :database)))

(defclass password-message (frontend-message)
  ((password :type string :initarg :password)))

(defclass query-message (frontend-message)
  ((query-string :type string :initarg :query-string)))

(defclass parse-message (frontend-message)
  ((destination-prepared-statement-name      :type string  :initarg :destination-prepared-statement-name)
   (query-string                             :type string  :initarg :query-string)
   (number-of-specified-parameter-data-types :type integer :initarg :number-of-specified-parameter-data-types)
   (parameter-data-type-oids                 :type list    :initarg :parameter-data-type-oids)))

(defclass bind-message (frontend-message)
  ((destination-portal-name              :type string  :initarg :destination-portal-name)
   (source-prepared-statement-name       :type string  :initarg :source-prepared-statement-name)
   (number-of-parameter-format-codes     :type integer :initarg :number-of-parameter-format-codes)
   (parameter-format-codes               :type list    :initarg :parameter-format-codes)
   (number-of-parameter-values           :type integer :initarg :number-of-parameter-values)
   (parameter-values                     :type list    :initarg :parameter-values)
   (number-of-result-column-format-codes :type integer :initarg :number-of-result-column-format-codes)
   (result-column-format-codes           :type list    :initarg :result-column-format-codes)))

(defclass parameter-value ()
  ((parameter-value-length :type integer                :initarg :parameter-value-length)
   (parameter-value        :type (vector unsigned-byte) :initarg :parameter-value)))

(defclass execute-message (frontend-message)
  ((portal-name                  :type string :initarg :portal-name)
   (max-number-of-rows-to-return :type integer :initarg :max-number-of-rows-to-return)))

(defclass sync-message (frontend-message) ())

(defclass describe-message (frontend-message)
  ((prepared-statement-or-portal      :type unsigned-byte :initarg :prepared-statement-or-portal)
   (prepared-statement-or-portal-name :type string        :initarg :prepared-statement-or-portal-name)))

(defclass close-message (frontend-message)
  ((prepared-statement-or-portal      :type unsigned-byte :initarg :prepared-statement-or-portal)
   (prepared-statement-or-portal-name :type string        :initarg :prepared-statement-or-portal-name)))

(defclass flush-message (frontend-message) ())

;;;
;;; Frontend message sending
;;;

(defgeneric message-length (frontend-message))

(defgeneric send-message (frontend-message))

(defmethod send-message :after ((frontend-message frontend-message))
  (finish-output *pg-stream*))

(defmethod message-length ((startup-message startup-message))
  (with-slots (user database) startup-message
    (+ 4
       4
       (+ (length "user")     1)
       (+ (length user)       1)
       (+ (length "database") 1)
       (+ (length database)   1)
       1)))

(defmethod send-message ((startup-message startup-message))
  (with-slots (user database) startup-message
    (write-int* 32 (message-length startup-message))
    (write-int* 32 196608)
    (write-string* "user")
    (write-string* user)
    (write-string* "database")
    (write-string* database)
    (write-byte* 0)))

(defmethod message-length ((password-message password-message))
  (with-slots (password) password-message
    (+ 4
       (+ (length password) 1))))

(defmethod send-message ((password-message password-message))
  (with-slots (password) password-message
    (write-byte* (char-code #\p))
    (write-int* 32 (message-length password-message))
    (write-string* password)))

(defmethod message-length ((query-message query-message))
  (with-slots (query-string) query-message
    (+ 4
       (+ (length query-string) 1))))

(defmethod send-message ((query-message query-message))
  (with-slots (query-string) query-message
    (write-byte* (char-code #\Q))
    (write-int* 32 (message-length query-message))
    (write-string* query-string)))

(defmethod message-length ((parse-message parse-message))
  (with-slots (destination-prepared-statement-name
               query-string
               number-of-specified-parameter-data-types
               parameter-data-type-oids)
      parse-message
    (+ 4
       (+ (length destination-prepared-statement-name) 1)
       (+ (length query-string)                        1)
       2
       (* (length parameter-data-type-oids) 4))))

(defmethod send-message ((parse-message parse-message))
  (with-slots (destination-prepared-statement-name
               query-string
               number-of-specified-parameter-data-types
               parameter-data-type-oids)
      parse-message
    (write-byte* (char-code #\P))
    (write-int* 32 (message-length parse-message))
    (write-string* destination-prepared-statement-name)
    (write-string* query-string)
    (write-int* 16 number-of-specified-parameter-data-types)
    (loop :for parameter-data-type-oid :in parameter-data-type-oids
       :do (write-int* 32 parameter-data-type-oid))))

(defmethod message-length ((bind-message bind-message))
  (with-slots (destination-portal-name
               source-prepared-statement-name
               number-of-parameter-format-codes
               parameter-format-codes
               number-of-parameter-values
               parameter-values
               number-of-result-column-format-codes
               result-column-format-codes)
      bind-message
    (+ 4
       (+ (length destination-portal-name)        1)
       (+ (length source-prepared-statement-name) 1)
       2
       (* (length parameter-format-codes) 2)
       2
       (loop :for parameter-value :in parameter-values
          :summing (+ 4 (length (slot-value parameter-value 'parameter-value))))
       2
       (* (length result-column-format-codes) 2))))

(defmethod send-message ((bind-message bind-message))
  (with-slots (destination-portal-name
               source-prepared-statement-name
               number-of-parameter-format-codes
               parameter-format-codes
               number-of-parameter-values
               parameter-values
               number-of-result-column-format-codes
               result-column-format-codes)
      bind-message
    (write-byte* (char-code #\B))
    (write-int* 32 (message-length bind-message))
    (write-string destination-portal-name)
    (write-string source-prepared-statement-name)
    (write-int* 16 number-of-parameter-format-codes)
    (loop :for parameter-format-code :in parameter-format-codes
       :do (write-int* 16 parameter-format-code))
    (write-int* 16 number-of-parameter-values)
    (loop :for parameter-value :in parameter-values
       :do (progn (write-int* 32 (slot-value parameter-value 'parameter-value-length))
                  (write-bytes* (slot-value parameter-value 'parameter-value))))
    (write-int* 16 number-of-result-column-format-codes)
    (loop :for result-column-format-code :in result-column-format-codes
       :do (write-int* 16 result-column-format-code))))

(defmethod message-length ((execute-message execute-message))
  (with-slots (portal-name max-number-of-rows-to-return) execute-message
    (+ 4
       (+ (length portal-name) 1)
       4)))

(defmethod send-message ((execute-message execute-message))
  (with-slots (portal-name max-number-of-rows-to-return) execute-message
    (write-byte* (char-code #\E))
    (write-int* 32 (message-length execute-message))
    (write-string* portal-name)
    (write-int* 32 max-number-of-rows-to-return)))

(defmethod message-length ((sync-message sync-message))
  4)

(defmethod send-message ((sync-message sync-message))
  (write-byte* (char-code #\S))
  (write-int* 32 (message-length sync-message)))

(defmethod message-length ((describe-message describe-message))
  (with-slots (prepared-statement-or-portal prepared-statement-or-portal-name)
      describe-message
    (+ 4
       1
       (+ (length prepared-statement-or-portal-name) 1))))

(defmethod send-message ((describe-message describe-message))
  (with-slots (prepared-statement-or-portal prepared-statement-or-portal-name)
      (write-byte* (char-code #\D))
    (write-int* 32 (message-length describe-message))
    (write-byte* prepared-statement-or-portal)
    (write-string* prepared-statement-or-portal-name)))

(defmethod message-length ((close-message close-message))
  (with-slots (prepared-statement-or-portal prepared-statement-or-portal-name)
      close-message
    (+ 4
       1
       (+ (length prepared-statement-or-portal-name) 1))))

(defmethod send-message ((close-message close-message))
  (with-slots (prepared-statement-or-portal prepared-statement-or-portal-name)
      (write-byte* (char-code #\D))
    (write-int* 32 (message-length close-message))
    (write-byte* prepared-statement-or-portal)
    (write-string* prepared-statement-or-portal-name)))

(defmethod message-length ((flush-message flush-message))
  4)

(defmethod send-message ((flush-message flush-message))
  (write-byte* (char-code #\H))
  (write-int* 32 (message-length flush-message)))

;;;
;;; Backend messages
;;;

(defclass backend-message (message) ())

(defmethod initialize-instance :after ((backend-message backend-message) &key)
  (log-debug "New instance of ~a~%" (type-of backend-message)))

(defclass error-response-message (backend-message)
  ((error-fields :type list :initarg :error-fields)))

(defclass error-field ()
  ((field-type-byte :type unsigned-byte :initarg :field-type-byte)
   (field-value     :type string        :initarg :field-value)))

(defclass notice-response-message (backend-message)
  ((notice-fields :type list :initarg :notice-fields)))

(defclass notice-field ()
  ((field-type-byte :type unsigned-byte :initarg :field-type-byte)
   (field-value     :type string        :initarg :field-value)))

(defclass authentication-ok-message (backend-message) ())

;; Protocol no longer supports it
;; (defclass authentication-kerberos-v5-message (backend-message) ())

(defclass authentication-cleartext-password-message (backend-message) ())

(defclass authentication-md5-password-message (backend-message)
  ((salt :type (vector unsigned-byte 4)
         :initarg :salt)))

;; Protocol only issued by pre-9.1 servers
;; (defclass authentication-scm-credential-message (backend-message) ())

(defclass authentication-gss-message (backend-message) ())

(defclass authentication-sspi-message (backend-message) ())

(defclass authentication-gss-continue-message (backend-message)
  ((authentication-data :type (vector unsigned-byte)
                        :initarg :authentication-data)))

(defclass negotiate-protocol-version-message (backend-message)
  ((newest-supported-minor-protocol-version :type integer
                                            :initarg :newest-supported-minor-protocol-version)
   (number-of-unrecognized-protocol-options :type integer
                                            :initarg :number-of-unrecognized-protocol-options)
   (unrecognized-protocol-options :type list
                                  :initarg :unrecognized-protocol-options)))

(defclass backend-key-data-message (backend-message)
  ((process-id :type integer :initarg :process-id)
   (secret-key :type integer :initarg :secret-key)))

(defclass parameter-status-message (backend-message)
  ((parameter-name  :type string :initarg :parameter-name)
   (parameter-value :type string :initarg :parameter-value)))

(defclass ready-for-query-message (backend-message)
  ((backend-transaction-status-indicator :type unsigned-byte
                                         :initarg :backend-transaction-status-indicator)))

(defclass command-complete-message (backend-message)
  ((command-tag :type string :initarg :command-tag)))

(defclass copy-in-response-message (backend-message)
  ((copy-format         :type integer :initarg :copy-format)
   (number-of-columns   :type integer :initarg :number-of-columns)
   (column-format-codes :type list    :initarg :column-format-codes)))

(defclass copy-out-response-message (backend-message)
  ((copy-format         :type integer :initarg :copy-format)
   (number-of-columns   :type integer :initarg :number-of-columns)
   (column-format-codes :type list    :initarg :column-format-codes)))

(defclass row-description-message (backend-message)
  ((number-of-fields-per-row :type integer :initarg :number-of-fields-per-row)
   (row-descriptions         :type list    :initarg :row-descriptions)))

(defclass row-description ()
  ((field-name              :type string  :initarg :field-name)
   (table-oid               :type integer :initarg :table-oid)
   (column-attribute-number :type integer :initarg :column-attribute-number)
   (data-type-oid           :type integer :initarg :data-type-oid)
   (data-type-size          :type integer :initarg :data-type-size)
   (type-modifier           :type integer :initarg :type-modifier)
   (format-code             :type integer :initarg :format-code)))

(defclass data-row-message (backend-message)
  ((number-of-column-values :type integer :initarg :number-of-column-values)
   (column-values           :type list    :initarg :column-values)))

(defclass data-row ()
  ((column-value-length :type integer                :initarg :column-value-length)
   (column-value        :type (vector unsigned-byte) :initarg :column-value)))

(defclass empty-query-response-message (backend-message) ())

(defclass portal-suspended-message (backend-message) ())

(defclass no-data-message (backend-message) ())

(defclass parameter-description-message (backend-message)
  ((number-of-parameters :type integer :initarg :number-of-parameters)
   (parameter-data-type-oids :type list :initarg :parameter-data-type-oids)))

(defclass close-complete-message (backend-message) ())

;;;
;;; Backend message parsing
;;;

(defun parse-remaining-error-response-message ()
  (make-instance 'error-response-message :error-fields
                 (loop :for field-type-byte = (read-byte*)
                    :if (eql 0 field-type-byte) :return error-fields
                    :else :collecting (make-instance 'error-field
                                                     :field-type-byte field-type-byte
                                                     :field-value (read-string*))
                    :into error-fields)))

(defun parse-remaining-notice-response-message ()
  (make-instance 'notice-response-message :notice-fields
                 (loop :for field-type-byte = (read-byte*)
                    :if (eql 0 field-type-byte) :return notice-fields
                    :else :collecting (make-instance 'notice-field
                                                     :field-type-byte field-type-byte
                                                     :field-value (read-string*))
                    :into notice-fields)))

(defun parse-startup-phase-1-response ()
  (let ((first-byte (read-byte*))
        (message-length (read-int* 32)))
    (cond ((eql first-byte (char-code #\E))
           (parse-remaining-error-response-message))
          ((eql first-byte (char-code #\R))
           (ecase (read-int* 32)
             (0 (make-instance 'authentication-ok-message))
             ;; (2 (make-instance 'authentication-kerberos-v5-message))
             (3 (make-instance 'authentication-cleartext-password-message))
             (5 (make-instance 'authentication-md5-password-message :salt (read-bytes* 4)))
             ;; (6 (make-instance 'authentication-scm-credential-message))
             (7 (make-instance 'authentication-gss-message))
             (9 (make-instance 'authentication-sspi-message))
             (8 (let ((remaining-bytes-count (- message-length 1 1 4)))
                  (make-instance 'authentication-gss-continue-message
                                 :authentication-data (read-bytes* remaining-bytes-count))))))
          ((eql first-byte (char-code #\v))
           (let* ((newest-supported-minor-protocol-version (read-int* 32))
                  (number-of-unrecognized-protocol-options (read-int* 32))
                  (unrecognized-protocol-options
                   (loop :for i :from 0 :below number-of-unrecognized-protocol-options
                      :collecting (read-string*))))
             (make-instance 'negotiate-protocol-version-message
                            :newest-supported-minor-protocol-version newest-supported-minor-protocol-version
                            :number-of-unrecognized-protocol-options number-of-unrecognized-protocol-options
                            :unrecognized-protocol-options unrecognized-protocol-options)))
          (t (error "Unexpected first byte ~a when parsing startup phase 1 response" first-byte)))))

(defun parse-startup-phase-2-response ()
  (let ((first-byte (read-byte*))
        (message-length (read-int* 32)))
    (declare (ignore message-length))
    (cond ((eql first-byte (char-code #\K))
           (make-instance 'backend-key-data-message
                          :process-id (read-int* 32)
                          :secret-key (read-int* 32)))
          ((eql first-byte (char-code #\S))
           (make-instance 'parameter-status-message
                          :parameter-name (read-string*)
                          :parameter-value (read-string*)))
          ((eql first-byte (char-code #\Z))
           (make-instance 'ready-for-query-message
                          :backend-transaction-status-indicator (read-byte*)))
          ((eql first-byte (char-code #\E))
           (parse-remaining-error-response-message))
          ((eql first-byte (char-code #\N))
           (parse-remaining-notice-response-message))
          (t (error "Unexpected first byte ~a when parsing startup phase 2 response" first-byte)))))

(defun parse-simple-query-response ()
  (let ((first-byte (read-byte*))
        (message-length (read-int* 32)))
    (declare (ignore message-length))
    (cond ((eql first-byte (char-code #\C))
           (make-instance 'command-complete-message :command-tag (read-string*)))
          ((eql first-byte (char-code #\G))
           (let* ((copy-format (read-int* 8))
                  (number-of-columns (read-int* 16))
                  (column-format-codes (read-ints* 16 number-of-columns)))
             (make-instance 'copy-in-response-message
                            :copy-format copy-format
                            :number-of-columns number-of-columns
                            :column-format-codes column-format-codes)))
          ((eql first-byte (char-code #\H))
           (let* ((copy-format (read-int* 8))
                  (number-of-columns (read-int* 16))
                  (column-format-codes (read-ints* 16 number-of-columns)))
             (make-instance 'copy-out-response-message
                            :copy-format copy-format
                            :number-of-columns number-of-columns
                            :column-format-codes column-format-codes)))
          ((eql first-byte (char-code #\T))
           (let ((number-of-fields-per-row (read-int* 16)))
             (make-instance 'row-description-message
                            :number-of-fields-per-row number-of-fields-per-row
                            :row-descriptions
                            (loop :for i :from 0 :below number-of-fields-per-row
                               :collecting (make-instance 'row-description
                                                          :field-name (read-string*)
                                                          :table-oid (read-int* 32)
                                                          :column-attribute-number (read-int* 16)
                                                          :data-type-oid (read-int* 32)
                                                          :data-type-size (read-int* 16)
                                                          :type-modifier (read-int* 32)
                                                          :format-code (read-int* 16))))))
          ((eql first-byte (char-code #\D))
           (let ((number-of-column-values (read-int* 16)))
             (make-instance 'data-row-message
                            :number-of-column-values number-of-column-values
                            :column-values
                            (loop :for i :from 0 :below number-of-column-values
                               :collecting (let* ((column-value-length (read-int* 32))
                                                  (column-value (read-bytes* column-value-length)))
                                             (make-instance 'data-row
                                                            :column-value-length column-value-length
                                                            :column-value column-value))))))
          ((eql first-byte (char-code #\I))
           (make-instance 'empty-query-response-message))
          ((eql first-byte (char-code #\E))
           (parse-remaining-error-response-message))
          ((eql first-byte (char-code #\Z))
           (make-instance 'ready-for-query-message
                          :backend-transaction-status-indicator (read-byte*)))
          ((eql first-byte (char-code #\N))
           (parse-remaining-notice-response-message))
          (t (error "Unexpected first byte ~a when parsing simple query response" first-byte)))))

(defun parse-extended-query-parse-response ()
  (let ((first-byte (read-byte*))
        (message-length (read-int* 32)))
    (declare (ignore message-length))
    (cond ((eql first-byte (char-code #\1))
           (make-instance 'parse-complete-message))
          ((eql first-byte (char-code #\E))
           (parse-remaining-error-response-message))
          (t (error "Unexpected first byte ~a when parsing extended query parse response" first-byte)))))

(defun parse-extended-query-bind-response ()
  (let ((first-byte (read-byte*))
        (message-length (read-int* 32)))
    (declare (ignore message-length))
    (cond ((eql first-byte (char-code #\2))
           (make-instance 'bind-complete-message))
          ((eql first-byte (char-code #\E))
           (parse-remaining-error-response-message))
          (t (error "Unexpected first byte ~a when parsing extended query bind response" first-byte)))))

(defun parse-extended-query-execute-response ()
  (let ((first-byte (read-byte*))
        (message-length (read-int* 32)))
    (declare (ignore message-length))
    (cond ((eql first-byte (char-code #\C))
           (make-instance 'command-complete-message :command-tag (read-string*)))
          ((eql first-byte (char-code #\G))
           (let* ((copy-format (read-int* 8))
                  (number-of-columns (read-int* 16))
                  (column-format-codes (read-ints* 16 number-of-columns)))
             (make-instance 'copy-in-response-message
                            :copy-format copy-format
                            :number-of-columns number-of-columns
                            :column-format-codes column-format-codes)))
          ((eql first-byte (char-code #\H))
           (let* ((copy-format (read-int* 8))
                  (number-of-columns (read-int* 16))
                  (column-format-codes (read-ints* 16 number-of-columns)))
             (make-instance 'copy-out-response-message
                            :copy-format copy-format
                            :number-of-columns number-of-columns
                            :column-format-codes column-format-codes)))
          ((eql first-byte (char-code #\D))
           (let ((number-of-column-values (read-int* 16)))
             (make-instance 'data-row-message
                            :number-of-column-values number-of-column-values
                            :column-values
                            (loop :for i :from 0 :below number-of-column-values
                               :collecting (let* ((column-value-length (read-int* 32))
                                                  (column-value (read-bytes* column-value-length)))
                                             (make-instance 'data-row
                                                            :column-value-length column-value-length
                                                            :column-value column-value))))))
          ((eql first-byte (char-code #\I))
           (make-instance 'empty-query-response-message))
          ((eql first-byte (char-code #\E))
           (parse-remaining-error-response-message))
          ((eql first-byte (char-code #\N))
           (parse-remaining-notice-response-message))
          ((eql first-byte (char-code #\s))
           (make-instance 'portal-suspended-message))
          (t (error "Unexpected first byte ~a when parsing extended query bind response" first-byte)))))

(defun parse-extended-query-describe-response ()
  (let ((first-byte (read-byte*))
        (message-length (read-int* 32)))
    (declare (ignore message-length))
    (cond ((eql first-byte (char-code #\T))
           (let ((number-of-fields-per-row (read-int* 16)))
             (make-instance 'row-description-message
                            :number-of-fields-per-row number-of-fields-per-row
                            :row-descriptions
                            (loop :for i :from 0 :below number-of-fields-per-row
                               :collecting (make-instance 'row-description
                                                          :field-name (read-string*)
                                                          :table-oid (read-int* 32)
                                                          :column-attribute-number (read-int* 16)
                                                          :data-type-oid (read-int* 32)
                                                          :data-type-size (read-int* 16)
                                                          :type-modifier (read-int* 32)
                                                          :format-code (read-int* 16))))))
          ((eql first-byte (char-code #\n))
           (make-instance 'no-data-message))
          ((eql first-byte (char-code #\E))
           (parse-remaining-error-response-message))
          ((eql first-byte (char-code #\t))
           (let ((number-of-parameters (read-int* 16)))
             (make-instance 'parameter-description-message
                            :number-of-parameters number-of-parameters
                            :parameter-data-type-oids
                            (loop :for i :from 0 :below number-of-parameters
                               :collecting (read-int* 32))))))))

(defun parse-extended-query-close-response ()
  (let ((first-byte (read-byte*))
        (message-length (read-int* 32)))
    (declare (ignore message-length))
    (cond ((eql first-byte (char-code #\3))
           (make-instance 'close-complete-message))
          ((eql first-byte (char-code #\E))
           (parse-remaining-error-response-message)))))

;;;
;;; Interface
;;;

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
                  (format t "~a ~a~%" severity message))))))))

(defun test ()
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (unwind-protect
         (progn (sb-bsd-sockets:socket-connect socket "/var/run/postgresql/.s.PGSQL.5432")
                (let* ((*pg-stream*
                        (sb-bsd-sockets:socket-make-stream socket :input t :output t :timeout 5 :element-type :default)))
                  (connect "presage" "presage" :password "presage")))
      (sb-bsd-sockets:socket-shutdown socket :direction :io)
      (sb-bsd-sockets:socket-close socket)
      (print "socket closed"))))
