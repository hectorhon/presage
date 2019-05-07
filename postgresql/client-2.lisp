;; (in-package :com.hon.postgresql)

;; (message-format "AuthenticationOk" :backend t :frontend nil
;;                 (byte 1 :default #\R)
;;                 (int 32 :default 8)
;;                 (int 32 :default 0))

;; (message-format "AuthenticationKerberosV5" :backend t :frontend nil
;;                 (byte 1 :default #\R)
;;                 (int 32 :default 8)
;;                 (int 32 :default 2))

;; (message-format "AuthenticationCleartextPassword" :backend t :frontend nil
;;                 (byte 1 :default #\R)
;;                 (int 32 :default 8)
;;                 (int 32 :default 3))

;; (message-format "AuthenticationMD5Password" :backend t :frontend nil
;;                 (byte 1 :default #\R)
;;                 (int 32 :default 12)
;;                 (int 32 :default 5)
;;                 (byte 4 :as salt))

;; (message-format "AuthenticationSCMCredential" :backend t :frontend nil
;;                 (byte 1 :default #\R)
;;                 (int 32 :default 8)
;;                 (int 32 :default 6))

;; (message-format "AuthenticationGSS" :backend t :frontend nil
;;                 (byte 1 :default #\R)
;;                 (int 32 :default 8)
;;                 (int 32 :default 7))

;; (message-format "AuthenticationSSPI" :backend t :frontend nil
;;                 (byte 1 :default #\R)
;;                 (int 32 :default 8)
;;                 (int 32 :default 9))

;; (message-format "AuthenticationGSSContinue" :backend t :frontend nil
;;                 (byte 1 :default #\R)
;;                 (int 32 :as length)
;;                 (int 32 :default 8)
;;                 (byte nil :as gssapi-or-sspi-authentication-data))

;; (message-format "BackendKeyData" :backend t :frontend nil
;;                 (byte 1 :default #\K)
;;                 (int 32 :default 12)
;;                 (int 32 :as process-id)
;;                 (int 32 :as secret-key))

(eval-when (:compile-toplevel)
  (defparameter *startup-message-message-format*
    '("StartupMessage" from-frontend
      fields ((int 32 as length derive-by (computing message-length))
              (int 32 derive-by (fixed-value 196608))
              (string derive-by (fixed-value "user"))
              (string as user)
              (string derive-by (fixed-value "database"))
              (string as database)
              (byte 1 derive-by (fixed-value #\0))))))

(eval-when (:compile-toplevel)
  (defparameter *bind-message-format*
    '("Bind" from-frontend
      fields ((byte 1 derive-by (fixed-value #\B))
              (int 32 as length derive-by (computing message-length))
              (string as destination-portal-name)
              (string as source-prepared-statement-name)

              (int 16 as parameter-format-codes-count derive-by (counting parameter-format-codes))
              (int-array 16 as parameter-format-codes)

              (int 16 as parameter-values-count derive-by (counting parameter-values))
              (zero-or-more ((int 32 as parameter-value-length-in-bytes derive-by (counting parameter-value))
                             (byte as parameter-value))
               as parameter-values)

              (int 16 as result-column-format-codes-count derive-by (counting result-column-format-codes))
              (int-array 16 as result-column-format-codes)))))

(eval-when (:compile-toplevel)
  (defun get* (list key &key (required t))
    "Get the value of a key from a (possibly malformed) property list."
    (let ((tail (member key list)))
      (if (and required (null tail))
          (error "Required key ~a is missing from ~a" key list)
          (second tail)))))

(eval-when (:compile-toplevel)
  (defun message-length (field-formats)
    "Given the field formats, generate the form to calculate the message length."
    (let ((field-lengths
           (mapcar (lambda (field-format)
                     (let ((arg-name (get* field-format 'as :required nil)))
                       (ecase (first field-format)
                         (int          (second field-format))
                         (byte         (if (integerp (second field-format))
                                           (second field-format)
                                           `(length ,arg-name)))
                         (string       (let ((derivation (get* field-format 'derive-by :required nil)))
                                         (if derivation
                                             (if (eq 'fixed-value (first derivation))
                                                 (1+ (length (second derivation)))
                                                 (error "Only fixed values are supported for string length derivation."))
                                             `(1+ (length ,arg-name)))))
                         (int-array    `(* ,(second field-format) (length ,arg-name)))
                         (zero-or-more (let* ((field-formats (second field-format))
                                              (args (mapcan (lambda (field-format)
                                                              (unless (member 'derive-by field-format)
                                                                (list (get* field-format 'as))))
                                                            field-formats)))
                                         `(reduce '+ (mapcar (lambda ,args
                                                               ,(message-length (second field-format)))
                                                             ,(get* field-format 'as))))))))
                   field-formats)))
      `(reduce '+ (list ,@field-lengths)))))

(eval-when (:compile-toplevel)
  (defun message-contents (field-formats)
    "Given the field formats, generate the form to send the message."
    `(progn ,@(loop :for field-format :in field-formats
                 :collect (let* ((arg-name      (get* field-format 'as        :required nil))
                                 (derivation    (get* field-format 'derive-by :required nil))
                                 (value (cond (derivation
                                               (destructuring-bind (derive-method info) derivation
                                                 (ecase derive-method
                                                   (counting `(length ,info))
                                                   (fixed-value info)
                                                   (computing (ecase info
                                                                (message-length (message-length field-formats)))))))
                                              (t arg-name))))
                            (ecase (first field-format)
                              (byte          `(format t "write byte ~a~%" ,value))
                              (int           (ecase (second field-format)
                                               (16 `(format t "write int16 ~a~%" ,value))
                                               (32 `(format t "write int32 ~a~%" ,value))))
                              (string        `(format t "write string ~a~%" ,value))
                              (int-array     `(format t "write int array ~a~%" ,value))
                              (zero-or-more  (let* ((field-formats (second field-format))
                                                    (args (mapcan (lambda (field-format)
                                                                    (unless (member 'derive-by field-format)
                                                                      (list (get* field-format 'as))))
                                                                  field-formats)))
                                               `(loop :for ,args :in ,value
                                                   :do ,(message-contents field-formats))))))))))

(defmacro send (message-format-symbol)
  (let* ((message-format (symbol-value message-format-symbol))
         (field-formats (get* message-format 'fields)))
    (message-contents field-formats)))



(defun send-bind-message (destination-portal-name
                          source-prepared-statement-name
                          parameter-format-codes
                          parameter-values
                          result-column-format-codes)
  (send *bind-message-format*))

(defun send-startup-message-message (user database)
  (send *startup-message-message-format*))
