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
  ((size            :type integer)
   (size-field-name :type symbol)
   (exact-value     :type byte)))



(defclass field-format ()
  ((field-name :type symbol            :initarg :field-name)
   (data-type  :type message-data-type :initarg :data-type)
   (derivation :type t                 :initarg :derivation)))

(defclass message-format ()
  ((format-name :type string                :initarg :format-name)
   (source      :type symbol                :initarg :source) ; 'backend or 'frontend
   (fields      :type (vector field-format) :initarg :fields)))

(make-instance 'message-format
               :format-name "StartupMessage"
               :source 'frontend
               :fields (vector (make-instance 'field-format
                                              :field-name 'length
                                              :data-type (make-instance 'integer* :bits 32)
                                              :derivation 'message-length)
                               (make-instance 'field-format
                                              :data-type (make-instance 'integer* :bits 32 :exact-value 196608))
                               123))

;; (make-instance 'integer*
;;                :bits 32
;;                :exact-value 196608)
;; (make-instance 'string*
;;                :exact-value "user")
;; (make-instance
