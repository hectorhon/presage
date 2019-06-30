(in-package :com.hon.utils.logging)

(let ((log-stream *standard-output*))
  (defun log-stream ()
    log-stream))

(defmacro log-debug (control-string &rest format-arguments)
  (let ((log-level (and (boundp '*package-log-level*)
                        (symbol-value '*package-log-level*))))
    (if (or (eq log-level 'debug)
            (eq log-level 'info)
            (eq log-level 'warn)
            (eq log-level 'error))
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
" (make-string (+ (length prefix) 2) :initial-element #\Space))))))))

(defmacro set-package-log-level (level)
  "Valid levels are nil, trace, debug, info, warn, error."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defparameter *package-log-level* ,level)))
