(in-package :com.hon.postgresql)

(message-format "AuthenticationOk" :backend t :frontend nil
                (byte 1 :default #\R)
                (int 32 :default 8)
                (int 32 :default 0))

(message-format "AuthenticationKerberosV5" :backend t :frontend nil
                (byte 1 :default #\R)
                (int 32 :default 8)
                (int 32 :default 2))

(message-format "AuthenticationCleartextPassword" :backend t :frontend nil
                (byte 1 :default #\R)
                (int 32 :default 8)
                (int 32 :default 3))

(message-format "AuthenticationMD5Password" :backend t :frontend nil
                (byte 1 :default #\R)
                (int 32 :default 12)
                (int 32 :default 5)
                (byte 4 :as salt))

(message-format "AuthenticationSCMCredential" :backend t :frontend nil
                (byte 1 :default #\R)
                (int 32 :default 8)
                (int 32 :default 6))

(message-format "AuthenticationGSS" :backend t :frontend nil
                (byte 1 :default #\R)
                (int 32 :default 8)
                (int 32 :default 7))

(message-format "AuthenticationSSPI" :backend t :frontend nil
                (byte 1 :default #\R)
                (int 32 :default 8)
                (int 32 :default 9))

(message-format "AuthenticationGSSContinue" :backend t :frontend nil
                (byte 1 :default #\R)
                (int 32 :as length)
                (int 32 :default 8)
                (byte nil :as gssapi-or-sspi-authentication-data))

(message-format "BackendKeyData" :backend t :frontend nil
                (byte 1 :default #\K)
                (int 32 :default 12)
                (int 32 :as process-id)
                (int 32 :as secret-key))

(defparameter *message-format*
  '(name "Bind" backend nil frontend t
    fields ((type (byte 1) default #\B)
            (type (int 32) as length derive-from message-length)
            (type string as destination-portal-name)
            (type string as source-prepared-statement-name)

            (type (int 16) as parameter-format-codes-count derive-by (counting parameter-format-codes))
            (type (int-array 16 length-from parameter-format-codes) as parameter-format-codes)

            (type (int 16) as parameter-values-count derive-by (counting parameter-values))
            (zero-or-more ((type (int 32) as parameter-value-length-in-bytes derive-by (counting parameter-value))
                           (type (byte parameter-value-length-in-bytes) as parameter-value))
             as parameter-values repeat-times parameter-values-count)

            (type (int 16) as result-column-format-codes-count derive-by (counting result-column-format-codes))
            (type (int-array 16 length-from result-column-format-codes) as result-column-format-codes))))

(defmacro message-length (fields)
  (let ((lengths (mapcar (lambda (field)
                           (let ((type (getf field 'type)))
                             (cond ((eq 'string type) `(length ,(getf field 'as)))
                                   ((consp type) (ecase (car type)
                                                   ((int byte) (second type))
                                                   (int-array `(* ,(second type)
                                                                  (length ,(getf type 'length-from))))))
                                   ((null type) 0) ;; zero-or-more type
                                   (t (error "xxx")))))
                         (eval fields))))
    `(reduce '+ (list ,@lengths))))

(defun send-bind-message (destination-portal-name
                          source-prepared-statement-name
                          parameter-format-codes
                          parameter-values
                          result-column-format-codes)
  (message-length (getf *message-format* 'fields)))
