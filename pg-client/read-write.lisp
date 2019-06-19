(in-package :com.hon.clients.postgresql)

(defun read-byte* ()
  (read-byte *standard-input*))

(defun read-bytes* (count)
  (loop :with arr = (make-array count :element-type 'unsigned-byte)
     :for i :from 0 :below count
     :do (setf (aref arr i) (read-byte*))
     :finally (return arr)))

(defun read-int* (bits)
  (bytes-to-integer (read-bytes* (/ bits 8))))

(defun read-ints* (bits count)
  (loop :for i :from 0 :below count
     :collect (read-int* bits)))

(defun read-string* ()
  (coerce (loop :for c = (read-char)
             :until (eql #\null c)
             :collect c :into chars
             :finally (return chars))
          'string))

(defun write-byte* (byte)
  (log-debug "write byte (1): ~a" byte)
  (write-byte byte *standard-output*))

(defun write-bytes* (bytes)
  (log-debug "write bytes (~d): ~a" (length bytes) bytes)
  (write-sequence bytes *standard-output*))

(defun write-int* (bits integer)
  (log-debug "write int~a (~d): ~a" bits (/ bits 8) integer)
  (write-sequence (integer-to-bytes integer bits) *standard-output*))

(defun write-string* (string)
  (log-debug "write string (~d): ~a" (length string) string)
  (write-string string)
  (write-byte* 0))
