(in-package :com.hon.utils.logging)

(let ((log-stream *standard-output*))
  (defun log-stream ()
    log-stream))

(defmacro log-debug (control-string &rest format-arguments)
  `(let ((log-entry
          (format nil ,control-string ,@format-arguments))
         (prefix
          (concatenate 'string "DEBUG "
                       ;; (package-name ,*package*)
                       )))
     (format (log-stream) "~a: ~a~%"
             prefix
             (replace-all log-entry "
" (concatenate 'string "
" (make-string (+ (length prefix) 2) :initial-element #\Space))))))
