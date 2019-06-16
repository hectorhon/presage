(in-package :com.hon.clients.postgresql)

(defclass backend-message (message) ())

(defmethod initialize-instance :after ((backend-message backend-message) &key)
  (log-debug "~a" backend-message))

(defclass error-response-message (backend-message)
  ((error-fields :type list :initarg :error-fields)))

(defclass error-field (slots-printable-for-debugging)
  ((field-type-byte :type unsigned-byte :initarg :field-type-byte)
   (field-value     :type string        :initarg :field-value)))

(defclass notice-response-message (backend-message)
  ((notice-fields :type list :initarg :notice-fields)))

(defclass notice-field (slots-printable-for-debugging)
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
   (field-descriptions       :type list    :initarg :field-descriptions       :reader field-descriptions)))

(defclass field-description (slots-printable-for-debugging)
  ((field-name              :type string  :initarg :field-name              :reader field-name)
   (table-oid               :type integer :initarg :table-oid)
   (column-attribute-number :type integer :initarg :column-attribute-number)
   (data-type-oid           :type integer :initarg :data-type-oid)
   (data-type-size          :type integer :initarg :data-type-size)
   (type-modifier           :type integer :initarg :type-modifier)
   (format-code             :type integer :initarg :format-code)))

(defclass data-row-message (backend-message)
  ((number-of-column-values :type integer :initarg :number-of-column-values)
   (column-values           :type list    :initarg :column-values)))

(defclass column-value (slots-printable-for-debugging)
  ((column-value-length :type integer                :initarg :column-value-length)
   (column-value-value  :type (vector unsigned-byte) :initarg :column-value-value)))

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
                            :field-descriptions
                            (loop :for i :from 0 :below number-of-fields-per-row
                               :collecting (make-instance 'field-description
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
                                             (make-instance 'column-value
                                                            :column-value-length column-value-length
                                                            :column-value-value column-value))))))
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
                                             (make-instance 'column-value
                                                            :column-value-length column-value-length
                                                            :column-value-value column-value))))))
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
                            :field-descriptions
                            (loop :for i :from 0 :below number-of-fields-per-row
                               :collecting (make-instance 'field-description
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
