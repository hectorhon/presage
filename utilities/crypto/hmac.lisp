;;;; https://tools.ietf.org/html/rfc2104

(in-package :com.hon.utils.crypto.hmac)

(set-package-log-level nil)

(defun compute-mac (hash-function block-size secret-key text)
  (let* ((ipad (make-array block-size :initial-element #x36))
         (opad (make-array block-size :initial-element #x5c))
         (step-1-output (let ((secret-key (if (> (length secret-key) block-size)
                                              (funcall hash-function secret-key)
                                              secret-key)))
                          (concatenate 'vector
                                       secret-key
                                       (make-array (- block-size (length secret-key))
                                                   :initial-element 0))))
         (step-2-output (map 'vector #'logxor step-1-output ipad))
         (step-3-output (concatenate 'vector step-2-output text))
         (step-4-output (funcall hash-function step-3-output))
         (step-5-output (map 'vector #'logxor step-1-output opad))
         (step-6-output (concatenate 'vector step-5-output step-4-output))
         (step-7-output (funcall hash-function step-6-output)))
    (log-debug "K0 is~%~a" (bytes-to-hex-string step-1-output))
    (log-debug "K0^ipad is~%~a" (bytes-to-hex-string step-2-output))
    (log-debug "Hash((Key^ipad)||text) is~%~a" (bytes-to-hex-string step-4-output))
    (log-debug "K0 xor opad is~%~a" (bytes-to-hex-string step-5-output))
    (log-debug "Hash((K0^opad)||Hash((K0^ipad)||text)) is~%~a" (bytes-to-hex-string step-7-output))
    step-7-output))

(defun compute-hmac-sha256 (secret-key text)
  (compute-mac #'com.hon.utils.crypto.sha256:compute-hash 64 secret-key text))

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
