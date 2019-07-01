;;;; https://tools.ietf.org/html/rfc8018

(in-package :com.hon.utils.crypto.pbkdf2)

(set-package-log-level nil)

(defvar *prf*)

(defun F (P S c i)
  (loop :with arr = (funcall *prf* P (concatenate 'vector S (integer-to-bytes i 32)))
     :for index :from 1 :to (1- c)
     :do (map-into arr #'logxor (funcall *prf* P arr))
     :finally (return arr)))

(defun compute-kdf (password salt iteration-count intended-derived-key-length prf-output-length)
  (let ((P password)
        (S salt)
        (c iteration-count)
        (dkLen intended-derived-key-length)
        (hLen prf-output-length))
    (if (> dkLen (* (1- (expt 2 32)) hLen))
        (error "intended-derived-key-length too long"))
    (multiple-value-bind (l r*) (ceiling dkLen hLen)
      (let ((r (+ hLen r*)))
        (declare (ignore r))
        (apply #'concatenate 'vector (loop :for index :from 1 :to l
                                        :collect (F P S c index)))))))

(defun compute-pbkdf2-hmac-sha256 (password salt iteration-count &optional (intended-derived-key-length 32))
  (let ((*prf* #'com.hon.utils.crypto.hmac:compute-hmac-sha256))
    (compute-kdf password salt iteration-count intended-derived-key-length 32)))

;;; https://tools.ietf.org/html/rfc7914#page-12

;; (check-equals "PBKDF2-HMAC-SHA-256 (P=\"passwd\", S=\"salt\",c=1, dkLen=64)"
;;               (integer-to-bytes #x55ac046e56e3089fec1691c22544b605f94185216dde0465e68b9d57c20dacbc49ca9cccf179b645991664b39d77ef317c71b845b1e30bd509112041d3a19783
;;                                 (* 64 8))
;;               (compute-pbkdf2-hmac-sha256 (map 'vector #'char-code "passwd")
;;                                           (map 'vector #'char-code "salt")
;;                                           1 64))

;; (check-equals "PBKDF2-HMAC-SHA-256 (P=\"Password\", S=\"NaCl\",c=80000, dkLen=64)"
;;               (integer-to-bytes #x4ddcd8f60b98be21830cee5ef22701f9641a4418d04c0414aeff08876b34ab56a1d425a1225833549adb841b51c9b3176a272bdebba1d078478f62b397f33c8d
;;                                 (* 64 8))
;;               (compute-pbkdf2-hmac-sha256 (map 'vector #'char-code "Password")
;;                                           (map 'vector #'char-code "NaCl")
;;                                           100 64))

(defun test (iterations)
  (compute-pbkdf2-hmac-sha256 (map 'vector #'char-code "Password")
                              (map 'vector #'char-code "NaCl")
                              iterations 64))

;; (progn (sb-profile:profile com.hon.utils.crypto.sha256:compute-hash
;;                            com.hon.utils.crypto.hmac:compute-hmac-sha256
;;                            com.hon.utils.crypto.pbkdf2:compute-pbkdf2-hmac-sha256)
;;        (time (com.hon.utils.crypto.pbkdf2::test 3000))
;;        (sb-profile:report))
