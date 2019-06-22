(in-package :com.hon.http.server)

(defparameter *http-response*
  "HTTP/1.1 200 OK
Cache-Control: no-store
Content-Length: 59

<html><head></head><body><h1>Hello world</h1></body></html>")

(defvar *threads* nil)

(defun serve ()
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (sockopt-reuse-address socket) t)
    (without-interrupts
      (unwind-protect
           (with-local-interrupts
             (progn (socket-bind socket (make-inet-address "127.0.0.1") 8080)
                    (socket-listen socket 4)
                    (loop
                       (multiple-value-bind (peer-socket peer-address) (socket-accept socket)
                         (declare (ignore peer-address))
                         (without-interrupts
                           (unwind-protect
                                (with-local-interrupts
                                  (let ((socket-stream
                                         (socket-make-stream peer-socket :input t :output t :timeout 5)))
                                    (print (parse-http-request socket-stream))
                                    (write-string *http-response* socket-stream)
                                    (finish-output socket-stream)))
                             (socket-shutdown peer-socket :direction :io)
                             (socket-close peer-socket)
                             (print "peer socket closed")))))))
        (socket-shutdown socket :direction :io)
        (socket-close socket)
        (print "host socket closed")))))

(defun start ()
  (if (null *threads*)
      (push (make-thread #'serve :name "com.hon.http.server") *threads*)
      (print "server already started")))

(defun stop ()
  (if (null *threads*)
      (print "server already stopped")
      (terminate-thread (pop *threads*))))
