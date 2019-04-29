(in-package :com.hon.byte-utils)

(defun write-int32 (int32 u8-stream)
  "Write a 32-bit integer to a byte stream."
  (loop :for b :downfrom 32 :above 0 :by 8
     :do (let ((byte-to-write (ldb (byte 8 (- b 8)) int32)))
           (write-byte byte-to-write u8-stream))))

(defun write-null-terminated-string (str u8-stream)
  "Write an ASCII string as a C-style (null-terminated) string to a byte stream."
  (write-string str u8-stream)
  (write-byte 0 u8-stream))
