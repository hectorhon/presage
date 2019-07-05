(in-package :com.hon.utils.byte)



(defun integer-to-bytes (integer bits)
  (loop :with byte-count = (/ bits 8)
     :with arr = (make-array byte-count :element-type '(unsigned-byte 8))
     :for i :from 0 :below byte-count
     :do (setf (aref arr i)
               (ldb (byte 8 (* (- (1- byte-count) i) 8)) integer))
     :finally (return arr)))

(check-equals "integer-to-bytes"
              #(0 0 1 1)
              (integer-to-bytes 257 32))

(defun integer-64-to-bytes (integer)
  (declare (optimize (speed 3) (safety 3) (debug 0)))
  (declare (type fixnum integer))       ; signed/unsigned..?
  (loop :with arr = (make-array 8 :element-type '(unsigned-byte 8))
     :for i fixnum :from 0 :below 8
     :do (setf (aref arr i)
               (ldb (byte 8 (* (- 8 1 i) 8)) integer))
     :finally (return arr)))

(check-equals "integer-64-to-bytes"
              #(0 0 0 0 1 0 1 1)
              (integer-64-to-bytes 16777473))

(defun integer-32-to-bytes (integer)
  (declare (optimize (speed 3) (safety 3) (debug 0)))
  (declare (type (unsigned-byte 32) integer))
  (loop :with arr = (make-array 4 :element-type '(unsigned-byte 8))
     :for i fixnum :from 0 :below 4
     :do (setf (aref arr i)
               (ldb (byte 8 (* (- 4 1 i) 8)) integer))
     :finally (return arr)))

(check-equals "integer-32-to-bytes"
              #(1 0 1 1)
              (integer-32-to-bytes 16777473))



(defun bytes-to-integer (bytes)
  (loop :for byte :across bytes
     :for pos :downfrom (1- (length bytes))
     :summing (ash byte (* pos 8))))

(check-equals "bytes-to-integer"
              65792
              (bytes-to-integer
               (coerce #(0 0 1 1 0) '(vector (unsigned-byte 8)))))

(defun bytes-to-integer-32 (bytes)
  (declare (optimize (speed 3) (safety 3) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8)) bytes))
  (loop :for byte :across bytes
     :for pos :of-type (integer 0 3)
     :downfrom (1- (the (integer 0 4) (length bytes)))
     :summing (the (unsigned-byte 32)
                   (ash (the (unsigned-byte 32) byte)
                        (* 8  pos)))
     :of-type (unsigned-byte 32)))

(check-equals "bytes-to-integer-32"
              65537
              (bytes-to-integer-32
               (coerce #(0 1 0 1) '(vector (unsigned-byte 8)))))



(defun bytes-to-hex-string (bytes)
  (with-output-to-string (stream)
    (loop :for byte :across bytes
       :do (format stream "~2,'0x" byte))))

(check-equals "bytes-to-hex-string"
              "1234ABCD"
              (bytes-to-hex-string #(#x12 #x34 #xab #xcd)))
