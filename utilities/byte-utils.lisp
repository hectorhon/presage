(in-package :com.hon.utils.byte)

(defun integer-to-bytes (integer bits)
  (loop :with byte-count = (/ bits 8)
     :with arr = (make-array byte-count :element-type 'unsigned-byte)
     :for i :from 0 :below byte-count
     :do (setf (aref arr i)
               (ldb (byte 8 (* (- (1- byte-count) i) 8)) integer))
     :finally (return arr)))

(check-equals "integer-to-bytes"
              #(0 0 1 1)
              (integer-to-bytes 257 32))

(defun bytes-to-integer (bytes)
  (loop :for byte :across bytes
     :for pos :downfrom (1- (length bytes))
     :summing (ash byte (* pos 8))))

(check-equals "bytes-to-integer"
              65792
              (bytes-to-integer #(0 0 1 1 0)))
