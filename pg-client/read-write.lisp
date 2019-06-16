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
  (write-byte byte *standard-output*))

(defun write-bytes* (bytes)
  (write-sequence bytes *standard-output*))

(defun write-int* (bits integer)
  (write-sequence (integer-to-bytes integer bits) *standard-output*))

(defun write-string* (string)
  (write-string string)
  (write-byte* 0))
