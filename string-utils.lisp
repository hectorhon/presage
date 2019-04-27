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

(check-equals "read-until-whitespace"
              "abc"
              (with-input-from-string (input-stream "  abc def ghi")
                (read-until-whitespace input-stream)))

(defun split-string (str delimiter &optional limit)
  (loop
     :for prev-pos = 0 :then (+ (length delimiter) pos)
     :for pos = (search delimiter str :start2 prev-pos)
     :with count = 0
     :collecting (prog1 (subseq str prev-pos pos) (incf count))
     :into parts
     :until (or (if (not (null limit)) (>= count limit))
                (null pos))
     :finally (if (not (null pos))
                  (return (append parts (list (subseq str (+ (length delimiter) pos)))))
                  (return parts))))

(check-equals "split-string"
              (list "abc" "def" "ghi")
              (split-string "abc def ghi" " "))

(check-equals "split-string with limit"
              (list "abc" "def ghi")
              (split-string "abc def ghi" " " 1))

(defun assoc-value (key alist &key (test #'equal))
  "For an alist with string keys, return the value of the specified key."
  (cdr (assoc key alist :test test)))

(check-equals "assoc-value"
              "the value"
              (assoc-value "the key"
                           (acons "the key" "the value" nil)))

(check-equals "assoc-value case insensitive"
              "the value"
              (assoc-value "the key"
                           (acons "the KEY" "the value" nil)
                           :test #'string-equal))
