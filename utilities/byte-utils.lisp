(in-package :com.hon.utils.byte)

(declaim (ftype (function (integer fixnum) (values (simple-array (unsigned-byte 8)) &optional))
                integer-to-bytes))

(defun integer-to-bytes (integer bits)
  (declare (optimize (speed 3) (safety 3) (debug 0)))
  (loop :with byte-count = (/ bits 8)
     :with arr = (make-array byte-count :element-type '(unsigned-byte 8))
     :for i :from 0 :below byte-count
     :do (setf (aref arr i)
               (ldb (byte 8 (* (- (1- byte-count) i) 8)) integer))
     :finally (return arr)))

(check-equals "integer-to-bytes"
              #(0 0 1 1)
              (integer-to-bytes 257 32))

(declaim (ftype (function ((simple-array (unsigned-byte 8)))
                          (values integer &optional))
                bytes-to-integer))

(defun bytes-to-integer (bytes)
  (declare (optimize (speed 3) (safety 3) (debug 0)))
  (loop :for byte :across bytes
     :for pos :downfrom (1- (length bytes))
     :summing (ash byte (* pos 8))))

(check-equals "bytes-to-integer"
              65792
              (bytes-to-integer
               (coerce #(0 0 1 1 0) '(vector (unsigned-byte 8)))))

(defun bytes-to-hex-string (bytes)
  (with-output-to-string (stream)
    (loop :for byte :across bytes
       :do (format stream "~2,'0x" byte))))

(check-equals "bytes-to-hex-string"
              "1234ABCD"
              (bytes-to-hex-string #(#x12 #x34 #xab #xcd)))
