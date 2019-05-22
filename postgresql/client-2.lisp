;; (in-package :com.hon.postgresql)

(eval-when (:compile-toplevel)
  (defparameter *message-formats* ()))

(defmacro define-message-format (symbol &body definition)
  `(eval-when (:compile-toplevel)
     (defparameter ,symbol (quote ,definition))
     (push ,symbol *message-formats*)))

(define-message-format *startup-message-message-format*
  "StartupMessage" from-frontend
  fields ((int 32 as length derive-by (computing message-length))
          (int 32 derive-by (fixed-value 196608))
          (string derive-by (fixed-value "user"))
          (string as user)
          (string derive-by (fixed-value "database"))
          (string as database)
          (byte 1 derive-by (fixed-value #\0))))

(define-message-format *authentication-md5-password-message-format*
  "AuthenticationMD5Password" from-backend
  fields ((byte 1 derive-by (fixed-value #\R))
          (int 32 as length derived-by (fixed-value 12))
          (int 32 derive-by (fixed-value 5))
          (byte 4 as salt)))

(eval-when (:compile-toplevel)
  (defparameter *authentication-md5-password-message-format*
    '("AuthenticationMD5Password" from-backend
      fields ((byte 1 derive-by (fixed-value #\R))
              (int 32 derive-by (fixed-value 12))
              (int 32 derive-by (fixed-value 5))
              (byte 4 as salt)))))

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
                         (int          (/ (second field-format) 8))
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



(eval-when (:compile-toplevel)
  (defun get-message-format-predefined-first-byte (message-format)
    "Does not work for StartupMessage, for which it will return NIL."
    (if (string= "StartupMessage" (first message-format))
        (return-from get-message-format-predefined-first-byte nil))
    (let* ((first-field-format     (first (get* message-format 'fields)))
           (first-field-derivation (or (get* first-field-format 'derive-by)
                                       (error "First byte in message formats should be predefined."))))
      (or (get* first-field-derivation 'fixed-value)
          (error "First byte in message formats should be predefined."))))
  (defparameter *message-formats-bytes-alist*
    (mapcar (lambda (message-format)
              (cons (get-message-format-predefined-first-byte message-format)
                    message-format))
            *message-formats*)))

(defun parse (stream)
  "Does not work for StartupMessage."
  (let* ((first-byte (read-byte stream))
         (candidates (mapcar 'second
                             (remove-if (lambda (entry) (not (eql (first entry) first-byte)))
                                        *message-formats-bytes-alist*))))
    (if (endp candidates)
        (error "First byte ~a does not match any known message formats" first-byte))
    (loop :with field-index = 1 ; because first byte in the field formats is already parsed
       :and parsed-fields = ()
       :do (flet ((get-current-field-format (message-format)
                    (nth field-index (get* message-format 'fields)))
                  (get-field-type (field-format)
                    (subseq field-format 0 2))
                  (get-field-name (field-format)
                    (get* field-format 'as))
                  (add-to-parsed-fields (key-value-pair)
                    (destructuring-bind (key value) key-value-pair
                      (push parsed-fields value)
                      (push parsed-fields key)))
                  (parse-current-field-from-stream ()
                    
                    ))
             (let* ((field-types (mapcar (lambda (candidate)
                                           (get-field-type (get-current-field-format candidate)))
                                         candidates))
                    (expected-field-type (first field-types)) ; expected-field-type example is (int 32)
                    (current-field-among-candidates-has-same-type-and-name
                     and (mapcar (lambda (field-type) (equal expected-field-type field-type)) field-types)))
               (if 
             (ensure-current-field-among-candidates-has-same-type-and-name)
             (add-to-parsed-fields (parse-current-field-from-stream))
             (incf field-index))
       :until (<= 1 (length candidates)))))



(defun send-bind-message (destination-portal-name
                          source-prepared-statement-name
                          parameter-format-codes
                          parameter-values
                          result-column-format-codes)
  (send *bind-message-format*))

(defun send-startup-message-message (user database)
  (send *startup-message-message-format*))
