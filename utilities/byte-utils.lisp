(in-package :com.hon.utils.byte)

(defun integer-to-bytes (integer bits)
  (loop :for pos :downfrom (- bits 8) :to 0 :by 8
     :collecting (ldb (byte 8 pos) integer)))

(check-equals "integer-to-bytes"
              '(0 0 1 1)
              (integer-to-bytes 257 32))

(defun bytes-to-integer (bytes)
  (loop :for byte :in (coerce bytes 'list)
     :for pos :downfrom (1- (length bytes))
     :summing (ash byte (* pos 8))))

(check-equals "bytes-to-integer"
              65792
              (bytes-to-integer '(0 0 1 1 0)))
