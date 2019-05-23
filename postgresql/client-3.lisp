(defmacro make (typ &body body)
  `(make-instance ,typ ,@body))

(eval-when (:compile-toplevel)
  (defun get* (list key &key (required t))
    "Get the value of a key from a (possibly malformed) property list."
    (let ((tail (member key list)))
      (if (and required (null tail))
          (error "Required key ~a is missing from ~a" key list)
          (second tail)))))

(eval-when (:compile-toplevel)
  (defclass message-data-type () ())

  (defclass integer* (message-data-type)
    ((bits        :type integer :initarg :bits)
     (exact-value :type integer :initarg :exact-value)))

  (defclass integer-array* (message-data-type)
    ((bits                    :type integer :initarg :bits)
     (array-length-field-name :type symbol  :initarg :array-length-field-name)))

  (defclass string* (message-data-type)
    ((exact-value :type string :initarg :exact-value)))

  (defclass byte* (message-data-type)
    ((size            :type integer           :initarg :size)
     (size-field-name :type symbol            :initarg :size-field-name)
     (exact-value     :type (unsigned-byte 8) :initarg :exact-value)))

  (defclass field-format ()
    ((field-name :type symbol            :initarg :field-name  :reader field-name)
     (data-type  :type message-data-type :initarg :data-type   :reader data-type)
     (derivation :type t                 :initarg :derivation)))

  (defclass message-format ()
    ((format-name :type string                :initarg :format-name :reader format-name)
     (source      :type symbol                :initarg :source) ; 'backend or 'frontend
     (fields      :type (vector field-format) :initarg :fields))))

(eval-when (:compile-toplevel)
  (defparameter *message-formats* ()))

(defmacro define-message-format (format-name source &rest field-formats)
  `(let ((message-format (make 'message-format
                           :format-name ,format-name :source ,source
                           :fields (vector ,@field-formats))))
     (push message-format *message-formats*)))

(defmacro field (field-name &body body)
  `(make 'field-format :field-name ,field-name ,@body))

(eval-when (:compile-toplevel)
  (define-message-format "StartupMessage" 'frontend
    (field 'length
      :data-type  (make 'integer* :bits 32)
      :derivation 'message-length)

    (field 'protocol-version-number
      :data-type (make 'integer* :bits 32 :exact-value 196608))

    (field nil   :data-type (make 'string* :exact-value "user"))
    (field 'user :data-type (make 'string*))
    (field nil   :data-type (make 'byte* :size 1 :exact-value 0))

    (field nil       :data-type (make 'string* :exact-value "database"))
    (field 'database :data-type (make 'string*))
    (field nil       :data-type (make 'byte* :size 1 :exact-value 0))))

(defmacro make-send-function (message-format)
  (let* ((message-format (eval message-format))
         (function-symbol (read-from-string (concatenate 'string "send-" (format-name message-format))))
         (args (map 'list 'field-name
                    (remove-if-not (lambda (field-format)
                                     (and (slot-boundp field-format 'field-name)
                                          (not (slot-boundp field-format 'derivation))
                                          (not (slot-boundp (data-type field-format) 'exact-value))))
                                   (slot-value message-format 'fields))))
         (body (map 'list (lambda (field-format)
                            `(format t "write a ~a~%" ,(format nil "~a" (type-of (data-type field-format)))))
                    (slot-value message-format 'fields))))
    `(defun ,function-symbol (&key ,@args) ,@body)))

(make-send-function (first *message-formats*))

(send-startupmessage :user "user1" :database "database1")
