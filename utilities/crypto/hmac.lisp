;;;; https://tools.ietf.org/html/rfc2104

(in-package :com.hon.utils.crypto.hmac)

(set-package-log-level nil)

(defmacro define-compute-hmac-function (function-name hash-function-name hash-output-length block-size)
  `(let ((arr-1 (make-array ,block-size :element-type '(unsigned-byte 8)))
         (arr-3 (make-array (+ ,block-size ,hash-output-length) :element-type '(unsigned-byte 8))))
     (defun ,function-name (secret-key text)
       (declare (optimize (speed 3) (safety 3) (debug 0)))
       (declare (type (simple-array (unsigned-byte 8)) secret-key text))
       (let ((arr-2 (make-array (+ ,block-size (length text)) :element-type '(unsigned-byte 8))))
         (if (> (length secret-key) ,block-size)
             (map-into arr-1 #'identity (,hash-function-name secret-key))
             (let ((i 0))
               (loop :for byte :across secret-key
                  :do (setf (aref arr-1 i) byte)
                  :do (incf i))
               (loop :until (eql i ,block-size)
                  :do (setf (aref arr-1 i) #x00)
                  :do (incf i))))        ; K in arr-1
         (loop :for i :from 0 :below ,block-size
            :do (setf (aref arr-2 i)     ; K XOR ipad in arr-2 beginning
                      (logxor #x36 (aref arr-1 i))))
         (loop :for i :from 0 :below ,block-size
            :do (setf (aref arr-3 i)     ; K XOR opad in arr-3 beginning
                      (logxor #x5c (aref arr-1 i))))
         (loop :for byte :across text    ; Fill up second half of arr-2
            :for i :from ,block-size
            :do (setf (aref arr-2 i) byte))
         (loop :for byte :across (the (simple-array (unsigned-byte 8))
                                      (,hash-function-name arr-2)) ; Fill up second half of arr-3
            :for i :from ,block-size
            :do (setf (aref arr-3 i) byte))
         (,hash-function-name arr-3)))))

(define-compute-hmac-function compute-hmac-sha256 com.hon.utils.crypto.sha256:compute-hash 32 64)

(check-equals "HMAC-SHA256 Sample message for keylen=blocklen"
              (integer-to-bytes #x8BB9A1DB9806F20DF7F77B82138C7914D174D59E13DC4D0169C9057B133E1D62 (* 32 8))
              (compute-hmac-sha256
               (integer-to-bytes #x000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F202122232425262728292A2B2C2D2E2F303132333435363738393A3B3C3D3E3F
                                 (* 64 8))
               (integer-to-bytes #x53616D706C65206D65737361676520666F72206B65796C656E3D626C6F636B6C656E
                                 (* 34 8))))
(check-equals "HMAC-SHA256 Sample message for keylen<blocklen"
              (integer-to-bytes #xA28CF43130EE696A98F14A37678B56BCFCBDD9E5CF69717FECF5480F0EBDF790
                                (* 32 8))
              (compute-hmac-sha256
               (integer-to-bytes #x000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F
                                 (* 32 8))
               (integer-to-bytes #x53616D706C65206D65737361676520666F72206B65796C656E3C626C6F636B6C656E
                                 (* 34 8))))

(check-equals "HMAC-SHA256 Sample message for keylen>blocklen" ; there's a typo in the docs
              (integer-to-bytes #xBDCCB6C72DDEADB500AE768386CB38CC41C63DBB0878DDB9C7A38A431B78378D
                                (* 32 8))
              (compute-hmac-sha256
               (integer-to-bytes #x000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F202122232425262728292A2B2C2D2E2F303132333435363738393A3B3C3D3E3F404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F60616263
                                 (* 100 8))
               (integer-to-bytes #x53616D706C65206D65737361676520666F72206B65796C656E3D626C6F636B6C656E
                                 (* 34 8))))

(check-equals "HMAC-SHA256 Sample message for keylen<blocklen, with truncated tag"
              (integer-to-bytes #x27A8B157839EFEAC98DF070B331D5936
                                (* 16 8))
              (let ((hmac (compute-hmac-sha256
                           (integer-to-bytes #x000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F202122232425262728292A2B2C2D2E2F30
                                             (* 49 8))
                           (integer-to-bytes #x53616D706C65206D65737361676520666F72206B65796C656E3C626C6F636B6C656E2C2077697468207472756E636174656420746167
                                             (* 54 8)))))
                (subseq hmac 0 16)))
