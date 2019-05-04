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

(eval-when (:compile-toplevel)
  (defparameter *message-format*
    '("Bind" from-frontend
      fields ((byte 1 default #\B)
              (int 32 as length derive-from message-length)
              (string as destination-portal-name)
              (string as source-prepared-statement-name)

              (int 16 as parameter-format-codes-count derive-by (counting parameter-format-codes))
              (int-array 16 as parameter-format-codes)

              (int 16 as parameter-values-count derive-by (counting parameter-values))
              (zero-or-more ((int 32 as parameter-value-length-in-bytes derive-by (counting parameter-value))
                             (byte as parameter-value))
               as parameter-values)

              (int 16 as result-column-format-codes-count derive-by (counting result-column-format-codes))
              (int-array 16 as result-column-format-codes))))

  (defun get* (list key &key (required t))
    "Get the value of a key from a (possibly malformed) property list."
    (let ((tail (member key list)))
      (if (and required (null tail))
          (error "Required key ~a is missing from ~a" key list)
          (second tail))))

  (defun field-length (field-format)
    (ecase (first field-format)
      ((byte int) (second field-format))
      (string `(length ,(get* field-format 'as)))
      (int-array 0)
      (zero-or-more 0))))

(defmacro message-length (message-format)
  (let ((field-lengths
         (mapcar 'field-length (get* (symbol-value message-format) 'fields))))
    `(reduce '+ (list ,@field-lengths))))

(defun send-bind-message (destination-portal-name
                          source-prepared-statement-name
                          parameter-format-codes
                          parameter-values
                          result-column-format-codes)
  (message-length *message-format*))
