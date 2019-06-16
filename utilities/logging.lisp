(in-package :com.hon.utils.logging)

(let ((log-stream *standard-output*))
  (defun log-debug (control-string &rest format-arguments)
    (let ((log-entry
           (with-output-to-string (log-entry)
             (apply #'format log-entry control-string
                    format-arguments))))
      (format log-stream "DEBUG: ~a~%"
              ;;              log-entry))))
              (replace-all log-entry "
" "
       ")))))
