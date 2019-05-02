(in-package :com.hon.postgresql)

(defclass message ()
  ()
  (:documentation "Formats are described in https://www.postgresql.org/docs/9.6/protocol-message-formats.html"))

(defgeneric send (message stream))

(defclass startup-message (message)
  ((message-length)
   (version :type (typexpand '(unsigned-byte 32))
            :initform 196608)
   (user :type string
         :initarg :user
         :initform (error "User slot is required")
         :accessor user)
   (database :type string
             :initarg :database
             :accessor database)))

(defmethod initialize-instance :after ((startup-message startup-message) &key)
  (with-slots (message-length version user database) startup-message
    (unless (slot-boundp startup-message 'database)
      (setf database user))
    (setf message-length (+ (/ 32 8)           ; message-length
                            (/ 32 8)           ; version
                            (1+ (length "user")) ; user parameter name, including null terminator
                            (1+ (length user)) ; user, including null terminator
                            (1+ (length "database")) ; database parameter name, including null terminator
                            (1+ (length database))) ; database, including null terminator
          )))

(defmethod send ((startup-message startup-message) stream)
  (with-slots (message-length version user database) startup-message
    (write-int32 message-length stream)
    (write-int32 version stream)
    (write-null-terminated-string "user" stream)
    (write-null-terminated-string user stream)
    (write-null-terminated-string "database" stream)
    (write-null-terminated-string database stream)))

(defun test2 ()
  (with-open-file (stream "test.bin" :direction :output :element-type :default :if-exists :supersede)
    (let ((message (make-instance 'startup-message :user "presage")))
      (break)
      (send message stream))))

(defun test ()
  (let ((socket (make-instance 'local-socket :type :stream)))
    (setf (sockopt-reuse-address socket) t)
    (unwind-protect
         (progn (socket-connect socket "/var/run/postgresql/.s.PGSQL.5432")
                (let* ((socket-stream
                        (socket-make-stream socket :input t :output t :timeout 5 :element-type :default))
                       (message
                        (make-instance 'startup-message :user "presage")))
                  (print "sending message...")
                  (send message socket-stream)
                  (finish-output socket-stream)
                  (print "message sent.")
                  (loop (progn (print "reading...")
                               (print (read-byte socket-stream))
                               (print "read a byte.")))))
      (socket-shutdown socket :direction :io)
      (socket-close socket)
      (print "socket closed"))))
