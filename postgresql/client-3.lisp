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

(eval-when (:compile-toplevel)
  (defun lists-equal-p (&rest lists)
    (let ((list1 (car lists))
          (lists (cdr lists)))
      (if list1
          (if lists
              (if (equal list1 (car lists))
                  (apply #'lists-equal-p lists))
              t)
          t))))

;;;
;;; Message data types
;;;

(eval-when (:compile-toplevel)

  (defclass message-data-type () ())

  (defclass integer* (message-data-type)
    ((bits        :type integer :initarg :bits        :reader bits)
     (exact-value :type integer :initarg :exact-value :reader exact-value)))

  (defclass integer-array* (message-data-type)
    ((bits                    :type integer :initarg :bits                    :reader bits)
     (array-length-field-name :type symbol  :initarg :array-length-field-name :reader array-length-field-name)))

  (defclass string* (message-data-type)
    ((exact-value :type string :initarg :exact-value :reader exact-value)))

  (defclass bytes* (message-data-type)
    ((size            :type integer           :initarg :size            :reader size)
     (size-field-name :type symbol            :initarg :size-field-name :reader size-field-name)
     (exact-value     :type (unsigned-byte 8) :initarg :exact-value     :reader exact-value))))

;;;
;;; Methods to extract type information from message-data-type
;;;

(eval-when (:compile-toplevel)

  (defgeneric type-information (message-data-type)
    (:documentation "Extract type information that can be used to compare whether two
                     fields are of the same type. This is needed because exact-value is
                     inside the message-data-type, which should not be used for the
                     comparison.")
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

  (defmethod type-information append ((message-data-type bytes*))
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
    (and (slot-exists-p (data-type field-format) 'exact-value)
         (slot-boundp (data-type field-format) 'exact-value)))

  (defun derived-field-p (field-format)
    (declare (field-format field-format))
    (slot-boundp field-format 'derivation))

  (defclass message-format ()
    ((format-name :type string                :initarg :format-name :reader format-name)
     (source      :type symbol                :initarg :source      :reader source) ; 'backend or 'frontend
     (fields      :type (vector field-format) :initarg :fields      :reader fields)))

  (defparameter *message-formats* ()))

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
      (field nil       :data-type (make 'bytes* :size 1 :exact-value 0)))

    (define-message-format "AuthenticationOk" 'backend
      (field 'identifier :data-type (make 'bytes*   :size 1  :exact-value (char-code #\R)))
      (field 'length     :data-type (make 'integer* :bits 32 :exact-value 8))
      (field nil         :data-type (make 'integer* :bits 32 :exact-value 0)))

    (define-message-format "AuthenticationMD5Password" 'backend
      (field 'identifier :data-type (make 'bytes*   :size 1  :exact-value (char-code #\R)))
      (field 'length     :data-type (make 'integer* :bits 32 :exact-value 12))
      (field nil         :data-type (make 'integer* :bits 32 :exact-value 5))
      (field 'salt       :data-type (make 'bytes*   :size 4)))

    (define-message-format "AuthenticationCleartextPassword" 'backend
      (field 'identifier :data-type (make 'bytes*   :size 1  :exact-value (char-code #\R)))
      (field 'length     :data-type (make 'integer* :bits 32 :exact-value 8))
      (field nil         :data-type (make 'integer* :bits 32 :exact-value 3)))

    (define-message-format "BackendKeyData" 'backend
      (field 'identifier :data-type (make 'bytes*   :size 1  :exact-value (char-code #\K)))
      (field 'length     :data-type (make 'integer* :bits 32 :exact-value 12))
      (field 'process-id :data-type (make 'integer* :bits 32))
      (field 'secret-key :data-type (make 'integer* :bits 32)))

    (define-message-format "Bind" 'frontend
      (field 'identifier :data-type (make 'bytes*   :size 1  :exact-value (char-code #\K)))
      (field 'length     :data-type (make 'integer* :bits 32))
      (field 'destination-portal-name        :data-type (make 'string*))
      (field 'source-prepared-statement-name :data-type (make 'string*))
      (field 'number-of-parameter-format-codes :data-type (make 'integer* :bits 16))
      (field 'parameter-format-codes
        :data-type (make 'integer-array* :bits 16
                         :array-length-field-name 'number-of-parameter-format-codes))
      (field 'number-of-parameter-values :data-type (make 'integer* :bits 16))
      ;; (repeatedly :times 'number-of-parameter-values
      ;;             (field 'length-of-parameter-value :data-type (make 'integer* :bits 16))
      ;;             (field 'parameter-value           :data-type (make 'bytes*   :size-field-name 'length-of-parameter-value)))
      (field 'number-of-result-column-format-codes :data-type (make 'integer* :bits 16))
      (field 'result-column-format-codes
        :data-type (make 'integer-array* :bits 16
                         :array-length-field-name 'number-of-result-column-format-codes)))))

;;;
;;; Methods to generate forms to write the value to PostgreSQL's stream
;;;

(eval-when (:compile-toplevel)

  (defgeneric form-to-write-in-pg-format (message-data-type value-or-symbol)
    (:documentation "Generate a form to write value to *standard-output* according to
                     the format specified by message-data-type."))

  (defmethod form-to-write-in-pg-format ((message-data-type integer*) value-or-symbol)
    `(loop :with bits-per-byte = 8      ; for the #'write-byte
        :for pos :downfrom (- ,(bits message-data-type) bits-per-byte) :to 0 :by bits-per-byte
        :do (write-byte (ldb (byte 8 pos) ,value-or-symbol) *standard-output*)))

  (defmethod form-to-write-in-pg-format ((message-data-type integer-array*) value-or-symbol)
    `(loop :for integer :across ,value-or-symbol
        :do ,(form-to-write-in-pg-format (make-instance 'integer* :bits (bits message-data-type)) 'integer)))

  (defmethod form-to-write-in-pg-format ((message-data-type string*) value-or-symbol)
    `(progn (write-string ,value-or-symbol *standard-output*)
            (write-byte 0 *standard-output*)))

  (defmethod form-to-write-in-pg-format ((message-data-type bytes*) value-or-symbol)
    `(write-byte ,value-or-symbol *standard-output*)))

;;;
;;; Send message
;;;

(defmacro make-send-functions ()
  (labels ((make-send-function (message-format)
             (let* ((function-symbol
                     (intern (string-upcase (concatenate 'string "send-" (format-name message-format)))))
                    (args (map 'list #'field-name
                               (remove-if (lambda (field-format)
                                            (or (fixed-field-p field-format)
                                                (derived-field-p field-format)))
                                          (slot-value message-format 'fields))))
                    (body (map 'list (lambda (field-format)
                                       (cond ((fixed-field-p field-format)
                                              (form-to-write-in-pg-format (data-type field-format)
                                                                          (exact-value (data-type field-format))))
                                             ((derived-field-p field-format)
                                              `(print "Derived field not implemented."))
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
;;; Methods to generate the form to read the value from PostgreSQL's stream
;;;

(eval-when (:compile-toplevel)

  (defgeneric form-to-read-in-pg-format (message-data-type)
    (:documentation "Generate the form to read value from *standard-input* according to
                     the format specified by message-data-type."))

  (defmethod form-to-read-in-pg-format (message-data-type)
    (error "form-to-read-in-pg-format for ~a not implemented." (type-of message-data-type)))

  (defmethod form-to-read-in-pg-format ((message-data-type integer*))
    `(loop :with bits-per-byte = 8       ; for the #'write-byte
        :for shift-count :downfrom (- ,(bits message-data-type) bits-per-byte) :to 0 :by bits-per-byte
        :summing (ash (read-byte *standard-input*) shift-count)))

  (defmethod form-to-read-in-pg-format ((message-data-type bytes*))
    (if (slot-boundp message-data-type 'size-field-name)
        (error "Not yet implemented."))
    `(loop :with arr = (make-array ,(size message-data-type))
        :for i :from 0 :below ,(size message-data-type)
        :do (setf (aref arr i) (read-byte *standard-input*))
        :finally (if (eql 1 (length arr))
                     (return (aref arr 0))
                     (return arr)))))

;;;
;;; Classes of received messages
;;;

(eval-when (:compile-toplevel)

  (defgeneric cl-type (message-data-type)
    (:documentation "Which CL type does this message-data-type map to?"))

  (defmethod cl-type ((message-data-type integer*)) 'integer)

  (defmethod cl-type ((message-data-type integer-array*)) '(vector integer))

  (defmethod cl-type ((message-data-type string*)) 'string)

  (defmethod cl-type ((message-data-type bytes*))
    ;; byte1 will be stored as single byte, byteN will be stored as vector
    t))

(defclass message () ())

(eval-when (:compile-toplevel)
  (defun class-name-from-message-format (message-format)
    (intern (string-upcase (concatenate 'string (format-name message-format) "-message")))))

(defmacro generate-classes-from-message-formats ()
  (flet ((generate-class-from-message-format (message-format)
           (let ((slots
                  (map 'list (lambda (field-format)
                               (with-slots (field-name data-type derivation) field-format
                                 `(,field-name :initarg ,(intern (string-upcase field-name) :keyword)
                                               :reader ,field-name
                                               :type ,(cl-type (data-type field-format)))))
                       (remove-if (lambda (field-format)
                                    (or (fixed-field-p field-format)
                                        (derived-field-p field-format)
                                        (null (field-name field-format))))
                                        ; FIXME Maybe derived fields like msg length should be in the class?
                                  (fields message-format)))))
             `(defclass ,(class-name-from-message-format message-format) (message) ,slots))))
    `(progn ,@(mapcar #'generate-class-from-message-format
                      (remove-if (lambda (message-format)
                                   (not (eq 'backend (source message-format))))
                                 *message-formats*)))))

(generate-classes-from-message-formats)

;;;
;;; Parse message from backend
;;;

(defmacro make-parse-function ()
  (let ((field-index 0))
    (labels ((make-parse-forms (candidate-message-formats)
               (cond ((endp candidate-message-formats)
                      (error "No more candidates - check message format definitions."))
                     ((endp (cdr candidate-message-formats))
                      ;; only one candidate left, generate forms to read remaining fields
                      (let* ((candidate (car candidate-message-formats))
                             (remaining-fields (subseq (fields candidate) field-index)))
                        `(,@(loop :for field :across remaining-fields
                               :collecting `(let ((value ,(form-to-read-in-pg-format (data-type field))))
                                              ;; let clause is needed for the side effect (read from stream)
                                              ,(if (field-name field)
                                                   `(setf values (acons (quote ,(field-name field)) value values))
                                                   `(declare (ignore value)))))
                            (let ((message (make-instance (quote ,(class-name-from-message-format candidate)))))
                              (loop :for pair :in values :do (setf (slot-value message (car pair)) (cdr pair))
                                 :finally (return message))))))
                     (t
                      (flet ((current-field (message-format)
                               (aref (fields message-format) field-index)))
                        ;; ensure next fields have same type and have fixed value specifications
                        (loop :for message-format :in candidate-message-formats
                           :unless (or (eq (type-information (data-type (current-field (car candidate-message-formats))))
                                           (type-information (data-type (current-field message-format))))
                                       (fixed-field-p (current-field message-format)))
                           :do (error "Expected candidates to have same message-data-types - check message format definitions."))
                        ;; sort and group candidates by fixed values
                        (let ((candidates-grouped-by-fixed-values
                               (loop :for message-format :in candidate-message-formats
                                  :with hashmap = (make-hash-table :test 'eq)
                                  :do (push message-format
                                            (gethash (exact-value (data-type (current-field message-format))) hashmap))
                                  :finally (return hashmap))))
                          `((let ((value ,(form-to-read-in-pg-format (data-type (current-field (car candidate-message-formats))))))
                              ,(progn (incf field-index)
                                      `(cond ,@(loop :for fixed-value :being :the :hash-keys :of candidates-grouped-by-fixed-values
                                                  :using (:hash-value candidates)
                                                  :collecting `((eql ,fixed-value value)
                                                                ,@(make-parse-forms candidates)))
                                             (t (error "When parsing field number ~D, expected one of ~a, but got ~a instead."
                                                       ,field-index ; 1-indexed; happens to work because of previous incf
                                                       (list ,@(loop :for key :being :the :hash-key :of candidates-grouped-by-fixed-values
                                                                  :collecting key))
                                                       value))))))))))))
      (let ((candidate-message-formats (remove-if (lambda (message-format)
                                                    (not (eq 'backend (source message-format))))
                                                  *message-formats*)))
        `(defun parse-message-from-backend ()
           (let ((values nil))
             ,@(make-parse-forms candidate-message-formats)))))))

(make-parse-function)
