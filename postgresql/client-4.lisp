(defun write-int32 (value)
  (loop :for pos :downfrom (- 32 8) :to 0 :by 8
     :do (write-byte (ldb (byte 8 pos) value) *standard-output*)))

(defun write-string* (str)
  (write-string str *standard-output*)
  (write-byte 0 *standard-output*))

(defun write-byte* (byt)
  (write-byte byt *standard-output*))

(defun read-byte* ()
  (read-byte *standard-input*))

(defun read-bytes (count)
  (loop :with arr = (make-array count :element-type 'unsigned-byte)
     :for i :from 0 :below count
     :do (setf (aref arr i) (read-byte*))))

(defun read-int32 ()
  (loop :for shift-count :downfrom (- 32 8) :to 0 :by 8
     :summing (ash (read-byte *standard-input*) shift-count)))

(defun read-string ()
  (coerce (loop :for c = (read-char)
             :if (eql #\null c) :return chars
             :else :collecting c :into chars)
          'string))

(defclass message () ())

(defclass frontend-message (message) ())

(defclass startup-message (frontend-message)
  ((user     :type string :initarg :user)
   (database :type string :initarg :database)))

(defclass password-message (frontend-message)
  ((password :type string :initarg :password)))

(defclass backend-message (message) ())

(defclass error-response-message (backend-message)
  ((error-fields :initarg :error-fields)))

(defclass notice-response-message (backend-message)
  ((error-fields :initarg :notice-fields)))

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

(defgeneric message-length (frontend-message))

(defgeneric send-message (frontend-message))

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
    (write-int32 (message-length startup-message))
    (write-int32 196608)
    (write-string* "user")
    (write-string* user)
    (write-string* "database")
    (write-string* database)
    (write-byte* 0)))

(defmethod message-length ((password-message password-message))
  (with-slots (password) password-message
    (+ 1
       4
       (+ (length password) 1))))

(defmethod send-message ((password-message password-message))
  (with-slots (password) password-message
    (write-byte* (char-code #\p))
    (write-int32 (message-length password-message))
    (write-string* password)))

(defun parse-remaining-error-response-message ()
  (make-instance 'error-response-message :error-fields
                 (loop :for field-type-byte = (read-byte*)
                    :if (eql 0 field-type-byte) :return error-fields
                    :else :collecting (list field-type-byte (read-string)) :into error-fields)))

(defun parse-remaining-notice-response-message ()
  (make-instance 'notice-response-message :notice-fields
                 (loop :for field-type-byte = (read-byte*)
                    :if (eql 0 field-type-byte) :return notice-fields
                    :else :collecting (list field-type-byte (read-string)) :into notice-fields)))

(defun parse-startup-phase-1-message ()
  (let ((first-byte (read-byte*))
        (message-length (read-int32)))
    (cond ((eql first-byte (char-code #\E))
           (parse-remaining-error-response-message))
          ((eql first-byte (char-code #\R))
           (ecase (read-int32)
             (0 (make-instance 'authentication-ok-message))
             ;; (2 (make-instance 'authentication-kerberos-v5-message))
             (3 (make-instance 'authentication-cleartext-password-message))
             (5 (make-instance 'authentication-md5-password-message :salt (read-byte*)))
             ;; (6 (make-instance 'authentication-scm-credential-message))
             (7 (make-instance 'authentication-gss-message))
             (9 (make-instance 'authentication-sspi-message))
             (8 (let ((remaining-bytes-count (- message-length 1 1 4)))
                  (make-instance 'authentication-gss-continue-message
                                 :authentication-data (read-bytes remaining-bytes-count))))))
          ((eql first-byte (char-code #\v))
           (let* ((newest-supported-minor-protocol-version (read-int32))
                  (number-of-unrecognized-protocol-options (read-int32))
                  (unrecognized-protocol-options
                   (loop :for i :from 0 :below number-of-unrecognized-protocol-options
                      :collecting (read-string))))
             (make-instance 'negotiate-protocol-version-message
                            :newest-supported-minor-protocol-version newest-supported-minor-protocol-version
                            :number-of-unrecognized-protocol-options number-of-unrecognized-protocol-options
                            :unrecognized-protocol-options unrecognized-protocol-options))))))

(defun parse-startup-phase-2-message ()
  (let ((first-byte (read-byte*))
        (message-length (read-int32)))
    (declare (ignore message-length))
    (cond ((eql first-byte (char-code #\K))
           (make-instance 'backend-key-data-message
                          :process-id (read-int32)
                          :secret-key (read-int32)))
          ((eql first-byte (char-code #\S))
           (make-instance 'parameter-status-message
                          :parameter-name (read-string)
                          :parameter-value (read-string)))
          ((eql first-byte (char-code #\Z))
           (make-instance 'ready-for-query-message :backend-transaction-status-indicator (read-byte*)))
          ((eql first-byte (char-code #\E))
           (parse-remaining-error-response-message))
          ((eql first-byte (char-code #\N))
           (parse-remaining-notice-response-message)))))
