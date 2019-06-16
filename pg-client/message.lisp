(in-package :com.hon.clients.postgresql)

(defclass slots-printable-for-debugging () ())

(defvar *indent* "")

(defmethod print-object ((object slots-printable-for-debugging) stream)
  (format stream "~a" *indent*)
  (print-unreadable-object (object stream :type t :identity nil)
    (let ((slot-names (mapcar #'sb-mop:slot-definition-name
                              (sb-mop:class-slots (class-of object)))))
      (loop :for slot-name :in slot-names
         :if (slot-boundp object slot-name)
         :do (let ((value (slot-value object slot-name))
                   (*indent* (concatenate 'string *indent* "  ")))
               (if (listp value)
                   (format stream "~%~a~(~a~): ~%~{~a~%~}"
                           *indent* slot-name value)
                   (format stream "~%~a~(~a~): ~a"
                           *indent* slot-name value)))))))

(defclass message (slots-printable-for-debugging) ())
