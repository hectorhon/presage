(in-package :com.hon.string-utils)

(defun whitespace-char-p (c)
  "Tests if a character is a whitespace."
  (or (char= #\Space c)
      (not (graphic-char-p c))))

(defun read-until (test input-stream)
  "Reads characters from input-stream until test is true, and returns
a string with those characters. If test is true at the beginning of
the input-stream, skip those characters before starting. If there are
no characters to process, signals end-of-file."
  (loop :for c = (peek-char nil input-stream nil) ; skip those at beginnning
     :while (and (not (null c))
                 (funcall test c))
     :do (read-char input-stream))
  (unless (listen input-stream)
    (error 'end-of-file :stream input-stream))
  (with-output-to-string (output-stream)
    (loop :for c = (peek-char nil input-stream nil)
       :until (or (null c)
                  (funcall test c))
       :do (write-char (read-char input-stream) output-stream))))

(defun read-until-whitespace (&optional (input-stream *standard-input*))
  "Skips whitespace at the beginning of input-stream if any, then
reads the first whitespace-delimited word as a string."
  (read-until #'whitespace-char-p input-stream))

;; (handler-case
;; (with-input-from-string (input-stream "a")
;;   (loop (print (read-until-whitespace input-stream))))
;; (end-of-file (e)
;;   (format t "End of file ~a~%" e)))
