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
  (defparameter *message-format*
    '("Bind" from-frontend
      fields ((byte 1 default #\B)
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
    (let ((field-lengths
           (mapcar (lambda (field-format)
                     (ecase (first field-format)
                       (int          (second field-format))
                       (byte         (if (integerp (second field-format))
                                         (second field-format)
                                         `(length ,(get* field-format 'as))))
                       (string       `(length ,(get* field-format 'as)))
                       (int-array    `(* ,(second field-format) (length ,(get* field-format 'as))))
                       (zero-or-more `(reduce '+ (mapcar (lambda (parameter-value) ; TODO Use &rest?
                                                           ,(message-length (second field-format)))
                                                         ,(get* field-format 'as))))))
                   field-formats)))
      `(reduce '+ (list ,@field-lengths)))))

(defmacro send (message-format)
  (let ((field-formats (get* (symbol-value message-format) 'fields)))
    `(progn ,@(loop :for field-format in field-formats
                 :collect (let* ((arg-name      (get* field-format 'as        :required nil))
                                 (default-value (get* field-format 'default   :required nil))
                                 (derivation    (get* field-format 'derive-by :required nil))
                                 (value (cond (derivation
                                               (destructuring-bind (derive-method info) derivation
                                                 (ecase derive-method
                                                   (counting `(length ,info))
                                                   (computing (ecase info
                                                                (message-length (message-length field-formats)))))))
                                              (default-value default-value)
                                              (t arg-name))))
                            (ecase (first field-format)
                              (byte `(format t "write byte ~a~%" ,value))
                              (int `(format t "write int ~a~%" ,value))
                              (string `(format t "write string ~a~%" ,value))
                              (int-array `(format t "write int array ~a~%" ,value))
                              (zero-or-more `(format t "zom~%")))))))) ; TODO

(defun send-bind-message (destination-portal-name
                          source-prepared-statement-name
                          parameter-format-codes
                          parameter-values
                          result-column-format-codes)
  (send *message-format*)
  (message-length (get* *message-format* 'fields)))
