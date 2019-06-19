(in-package :com.hon.clients.postgresql)

(defclass frontend-message (message) ())

(defgeneric message-length (frontend-message))

(defgeneric send-message (frontend-message))

(defmethod send-message :after ((frontend-message frontend-message))
  (finish-output)
  (log-debug "Sent message ~a" (type-of frontend-message)))

;;;
;;; StartupMessage
;;;

(defclass startup-message (frontend-message)
  ((user     :type string :initarg :user)
   (database :type string :initarg :database)))

(defmethod message-length ((startup-message startup-message))
  (with-slots (user database) startup-message
    (+ 4
       4
       (1+ (length "user"))
       (1+ (length user))
       (1+ (length "database"))
       (1+ (length database))
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

;;;
;;; PasswordMessage
;;;

(defclass password-message (frontend-message)
  ((password :type string :initarg :password)))

(defmethod message-length ((password-message password-message))
  (with-slots (password) password-message
    (+ 4
       (1+ (length password)))))

(defmethod send-message ((password-message password-message))
  (with-slots (password) password-message
    (write-byte* (char-code #\p))
    (write-int* 32 (message-length password-message))
    (write-string* password)))

;;;
;;; Query
;;;

(defclass query-message (frontend-message)
  ((query-string :type string :initarg :query-string)))

(defmethod message-length ((query-message query-message))
  (with-slots (query-string) query-message
    (+ 4
       (1+ (length query-string)))))

(defmethod send-message ((query-message query-message))
  (with-slots (query-string) query-message
    (write-byte* (char-code #\Q))
    (write-int* 32 (message-length query-message))
    (write-string* query-string)))

;;;
;;; Parse
;;;

(defclass parse-message (frontend-message)
  ((destination-prepared-statement-name      :type string  :initarg :destination-prepared-statement-name)
   (query-string                             :type string  :initarg :query-string)
   (number-of-specified-parameter-data-types :type integer :initarg :number-of-specified-parameter-data-types)
   (parameter-data-type-oids                 :type list    :initarg :parameter-data-type-oids)))

(defmethod message-length ((parse-message parse-message))
  (with-slots (destination-prepared-statement-name
               query-string
               number-of-specified-parameter-data-types
               parameter-data-type-oids)
      parse-message
    (+ 4
       (1+ (length destination-prepared-statement-name))
       (1+ (length query-string))
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

;;;
;;; Bind
;;;

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
       (1+ (length destination-portal-name))
       (1+ (length source-prepared-statement-name))
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
    (write-string* destination-portal-name)
    (write-string* source-prepared-statement-name)
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

;;;
;;; Execute
;;;

(defclass execute-message (frontend-message)
  ((portal-name                  :type string :initarg :portal-name)
   (max-number-of-rows-to-return :type integer :initarg :max-number-of-rows-to-return)))

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

;;;
;;; Sync
;;;

(defclass sync-message (frontend-message) ())

(defmethod message-length ((sync-message sync-message))
  4)

(defmethod send-message ((sync-message sync-message))
  (write-byte* (char-code #\S))
  (write-int* 32 (message-length sync-message)))

;;;
;;; Describe
;;;

(defclass describe-message (frontend-message)
  ((prepared-statement-or-portal      :type unsigned-byte :initarg :prepared-statement-or-portal)
   (prepared-statement-or-portal-name :type string        :initarg :prepared-statement-or-portal-name)))

(defmethod message-length ((describe-message describe-message))
  (with-slots (prepared-statement-or-portal prepared-statement-or-portal-name)
      describe-message
    (+ 4
       1
       (+ (length prepared-statement-or-portal-name) 1))))

(defmethod send-message ((describe-message describe-message))
  (with-slots (prepared-statement-or-portal prepared-statement-or-portal-name)
      describe-message
    (write-byte* (char-code #\D))
    (write-int* 32 (message-length describe-message))
    (write-byte* prepared-statement-or-portal)
    (write-string* prepared-statement-or-portal-name)))

;;;
;;; Close
;;;

(defclass close-message (frontend-message)
  ((prepared-statement-or-portal      :type unsigned-byte :initarg :prepared-statement-or-portal)
   (prepared-statement-or-portal-name :type string        :initarg :prepared-statement-or-portal-name)))

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

;;;
;;; Flush
;;;

(defclass flush-message (frontend-message) ())

(defmethod message-length ((flush-message flush-message))
  4)

(defmethod send-message ((flush-message flush-message))
  (write-byte* (char-code #\H))
  (write-int* 32 (message-length flush-message)))
