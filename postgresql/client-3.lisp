;;;
;;; Utilities
;;;

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        (lambda (&rest args)
          (reduce #'funcall fns
                  :from-end t
                  :initial-value (apply fn1 args))))
      #'identity))

(defun lists-equal-p (&rest lists)
  (let ((list1 (car lists))
        (lists (cdr lists)))
    (if list1
        (if lists
            (if (equal list1 (car lists))
                (apply #'lists-equal-p lists))
            t)
        t)))

;;;
;;; Message data types
;;;

(eval-when (:compile-toplevel)

  (defclass message-data-type () ())

  (defclass integer* (message-data-type)
    ((bits        :type integer :initarg :bits        :reader bits)
     (exact-value :type integer :initarg :exact-value :reader exact-value)))

  (defclass integer-array* (message-data-type)
    ((bits                    :type integer :initarg :bits)
     (array-length-field-name :type symbol  :initarg :array-length-field-name)))

  (defclass string* (message-data-type)
    ((exact-value :type string :initarg :exact-value :reader exact-value)))

  (defclass byte* (message-data-type)
    ((size            :type integer           :initarg :size)
     (size-field-name :type symbol            :initarg :size-field-name)
     (exact-value     :type (unsigned-byte 8) :initarg :exact-value     :reader exact-value))))

;;;
;;; Methods to extract type information from message-data-type
;;;

(eval-when (:compile-toplevel)

  (defgeneric type-information (message-data-type)
    (:documentation "Extract type information that can be used to compare whether two
                     fields are of the same type. This is needed because exact-value is
                     inside the message-data-type...")
    (:method-combination append))       ; technically, can use list method
                                        ; combination. append preserves semantics

  (defmethod type-information append ((message-data-type message-data-type))
             (list (type-of message-data-type)))

  (defmethod type-information :around ((message-data-type message-data-type))
             (let ((result (call-next-method)))
               (or (cdr result)
                   (error "Specialization of type-information for all subclasses is
                           required for correct results."))))

  (defmethod type-information append ((message-data-type integer*))
             (list (bits message-data-type)))

  (defmethod type-information append ((message-data-type integer-array*))
             (list (bits message-data-type)
                   (gensym)))           ; type comparison not supported

  (defmethod type-information append ((message-data-type string*))
             (list (gensym)))           ; always unique, since the length isn't known

  (defmethod type-information append ((message-data-type byte*))
             ;; (list (size message-data-type))))
             (list (gensym))))          ; not implemented

;;;
;;; Message format
;;;

(eval-when (:compile-toplevel)

  (defclass field-format ()
    ((field-name :type symbol            :initarg :field-name :reader field-name)
     (data-type  :type message-data-type :initarg :data-type  :reader data-type)
     (derivation :type t                 :initarg :derivation)))

  (defun fixed-field-p (field-format)
    (declare (field-format field-format))
    (slot-boundp (data-type field-format) 'exact-value))

  (defun derived-field-p (field-format)
    (declare (field-format field-format))
    (slot-boundp field-format 'derivation))

  (defclass message-format ()
    ((format-name :type string                :initarg :format-name :reader format-name)
     (source      :type symbol                :initarg :source      :reader source) ; 'backend or 'frontend
     (fields      :type (vector field-format) :initarg :fields      :reader fields)))

  (defparameter *message-formats* ())

  (defun identifying-byte (message-format)
    (flet ((identifier-p (field-format)
             (eq 'identifier (field-name field-format))))
      (exact-value (data-type (find-if #'identifier-p (fields message-format)))))))

(defmacro define-message-format (format-name source &rest field-formats)
  `(let ((message-format
          (make-instance 'message-format
                         :format-name ,format-name :source ,source
                         :fields (vector ,@field-formats))))
     (push message-format *message-formats*)))

(defmacro field (field-name &body body)
  `(make-instance 'field-format :field-name ,field-name ,@body))

;;;
;;; Message format definitions
;;;

(eval-when (:compile-toplevel)

  (macrolet ((make (&rest args) `(make-instance ,@args)))

    (define-message-format "StartupMessage" 'frontend
      (field 'length   :data-type (make 'integer* :bits 32) :derivation 'message-length)
      (field 'protocol-version-number :data-type (make 'integer* :bits 32 :exact-value 196608))
      (field nil       :data-type (make 'string* :exact-value "user"))
      (field 'user     :data-type (make 'string*))
      (field nil       :data-type (make 'string* :exact-value "database"))
      (field 'database :data-type (make 'string*))
      (field nil       :data-type (make 'byte* :size 1 :exact-value 0)))

    (define-message-format "AuthenticationMD5Password" 'backend
      (field 'identifier :data-type (make 'byte*    :size 1  :exact-value (char-code #\R)))
      (field 'length     :data-type (make 'integer* :bits 32 :exact-value 12))
      (field nil         :data-type (make 'integer* :bits 32 :exact-value 5))
      (field 'salt       :data-type (make 'byte*    :size 4)))

    (define-message-format "AuthenticationCleartextPassword" 'backend
      (field 'identifier :data-type (make 'byte*    :size 1  :exact-value (char-code #\R)))
      (field 'length     :data-type (make 'integer* :bits 32 :exact-value 8))
      (field nil         :data-type (make 'integer* :bits 32 :exact-value 3)))))

;;;
;;; Methods to generate forms to write the value to PostgreSQL's stream
;;;

(eval-when (:compile-toplevel)

  (defgeneric form-to-write-in-pg-format (message-data-type value-or-symbol)
    (:documentation "Generate a form to write value to *standard-output* according to
                     the format specified by message-data-type."))

  ;; (defmethod form-to-write-in-pg-format ((message-data-type message-data-type) value-or-symbol)
  ;;   `(format t "form-to-write-in-pg-format not yet implemented~%"))

  (defmethod form-to-write-in-pg-format ((message-data-type integer*) value-or-symbol)
    `(loop :with bits-per-byte = 8      ; for the #'write-byte
        :for pos :downfrom (- ,(bits message-data-type) bits-per-byte) :to 0 :by bits-per-byte
        :do (write-byte (ldb (byte 8 pos) ,value-or-symbol) *standard-output*)))

  (defmethod form-to-write-in-pg-format ((message-data-type string*) value-or-symbol)
    `(progn (write-string ,value-or-symbol *standard-output*)
            (write-byte 0 *standard-output*)))

  (defmethod form-to-write-in-pg-format ((message-data-type byte*) value-or-symbol)
    `(write-byte ,value-or-symbol *standard-output*)))

;;;
;;; Send message
;;;

(defmacro make-send-functions ()
  (labels ((make-send-function (message-format)
             (let* ((function-symbol
                     (intern (string-upcase (concatenate 'string "send-" (format-name message-format)))))
                    (args (map 'list 'field-name
                               (remove-if (lambda (field-format)
                                            (or (fixed-field-p field-format)
                                                (derived-field-p field-format)))
                                          (slot-value message-format 'fields))))
                    (body (map 'list (lambda (field-format)
                                       ;; `(format t "write a ~a~%" ,(format nil "~a" (type-of (data-type field-format)))))
                                       (cond ((fixed-field-p field-format)
                                              (form-to-write-in-pg-format (data-type field-format)
                                                                          (exact-value (data-type field-format))))
                                             ((derived-field-p field-format)
                                              `(print "derived field not yet implemented"))
                                             (t
                                              (form-to-write-in-pg-format (data-type field-format)
                                                                          (field-name field-format)))))
                               (slot-value message-format 'fields))))
               `(defun ,function-symbol ,args ,@body))))
    (let ((frontend-message-formats
           (remove-if (lambda (message-format)
                        (not (eq 'frontend (source message-format))))
                      *message-formats*)))
      `(progn ,@(mapcar #'make-send-function frontend-message-formats)))))

(make-send-functions)

;;;
;;; Methods to read the value from PostgreSQL's stream
;;;

(defgeneric read-in-pg-format (message-data-type)
  (:documentation "Read value from *standard-input* according to the format specified by
                   message-data-type."))

(defmethod read-in-pg-format ((message-data-type integer*))
  (loop :with bits-per-byte = 8       ; for the #'write-byte
     :for shift-count :downfrom (- (bits message-data-type) bits-per-byte) :to 0 :by bits-per-byte
     :summing (ash (read-byte *standard-input*) shift-count)))

(defmethod read-in-pg-format ((message-data-type byte*))
  123) ; FIXME problem with size-field-name...

;;;
;;; Classes of received messages
;;;

(defclass message () ())

(eval-when (:compile-toplevel)
  (defun generate-class-name-from-message-format (message-format)
    (intern (string-upcase (concatenate 'string (format-name message-format) "-message")))))

(defmacro generate-classes-from-message-formats ()
  (flet ((generate-class-from-message-format (message-format)
           (let ((slots
                  (map 'list (lambda (field-format)
                               (with-slots (field-name data-type derivation) field-format
                                 `(,field-name :initarg ,(intern (string-upcase field-name) :keyword)
                                               :reader ,field-name)))
                       (remove-if (lambda (field-format)
                                    (or (fixed-field-p field-format)
                                        (derived-field-p field-format)
                                        (null (field-name field-format))))
                                        ; FIXME Maybe derived fields like msg length should be in the class?
                                  (fields message-format)))))
             `(defclass ,(generate-class-name-from-message-format message-format) (message) ,slots))))
    `(progn ,@(mapcar #'generate-class-from-message-format
                      (remove-if (lambda (message-format)
                                   (not (eq 'backend (source message-format))))
                                 *message-formats*)))))

(generate-classes-from-message-formats)

;;;
;;; Parse message from backend
;;;

(defun parse-message-from-backend (stream)
  (let* ((first-byte (read-byte stream))
         (candidates (remove-if (lambda (message-format)
                                  (or (not (eq 'backend (source message-format)))
                                      (not (eql first-byte (identifying-byte message-format)))))
                                *message-formats*))
         (field-index 1))               ; skipping the field with the identifying byte
    (unless candidates (error "No candidates for message format - check message format definitions."))
    (loop :until (and (car candidates) (not (cdr candidates)))
       :with next-data-type = nil       ;
       :and read-value = nil            ; the value that was read from the stream
       :do (flet ((ensure-next-fields-have-same-type ()
                    (let* ((next-fields (mapcar (lambda (message-format)
                                                  (aref (fields message-format) field-index))
                                                candidates))
                           (next-data-types (mapcar #'data-type next-fields)))
                      (unless (lists-equal-p (mapcar #'type-information next-data-types))
                        (error "Ambiguous message formats - check message format definitions."))
                      (unless (slot-boundp (first next-data-types) 'exact-value)
                        (error "Expected to determine message format by elimination based on a fixed value"))
                      (setf next-data-type (first next-data-types))))
                  (read-value-of-next-field-from-stream ()
                    (setf read-value (read-in-pg-format next-data-type)))
                  (filter-candidates-using-read-value ()
                    (setf candidates (remove-if (lambda (message-format)
                                                  (not (eql (exact-value (data-type (aref (fields message-format) field-index)))
                                                            read-value)))
                                                candidates))))
             (ensure-next-fields-have-same-type)
             (read-value-of-next-field-from-stream)
             (filter-candidates-using-read-value)
             (incf field-index))
       :if (null candidates)
       :do (error "No more candidates for message format - check message format definitions."))
    (let* ((candidate-message-format (car candidates))
           (message-class (generate-class-name-from-message-format candidate-message-format))
           (initargs (loop :for i :from field-index :below (length (fields candidate-message-format))
                        :appending (let* ((current-field-format (aref (fields candidate-message-format) i)))
                                     (if (not (or (derived-field-p current-field-format)
                                                  (fixed-field-p current-field-format)))
                                         (let* ((initarg (intern (string-upcase (field-name current-field-format)) :keyword))
                                                (value (read-in-pg-format (data-type current-field-format))))
                                           (list initarg value)))))))
      (print `(make (quote ,message-class) ,@initargs))
      (eval `(make (quote ,message-class) ,@initargs)))))
