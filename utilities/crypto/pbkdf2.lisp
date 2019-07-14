;;;; https://tools.ietf.org/html/rfc8018

(in-package :com.hon.utils.crypto.pbkdf2)

(set-package-log-level nil)

(declaim (type (function ((simple-array (unsigned-byte 8))
                          (simple-array (unsigned-byte 8))
                          (simple-array (unsigned-byte 8)))
                         (values (simple-array (unsigned-byte 8)) &optional))
               *prf*))

(defvar *prf*)

(declaim (type (integer 0 512) *prf-output-length*)) ; arbitrary

(defvar *prf-output-length*)

(declaim (inline F))

(defun F (P S c i)
  (declare (optimize (speed 3) (space 0) (safety 3) (debug 0) (compilation-speed 0)))
  (declare (type (simple-array (unsigned-byte 8)) P S))
  (declare (type fixnum c i))
  (let ((U_index (make-array *prf-output-length* :element-type '(unsigned-byte 8)))
        (result (make-array *prf-output-length* :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent U_index))
    (funcall *prf* P
             (concatenate '(vector (unsigned-byte 8))
                          S
                          (integer-to-bytes i 32))
             U_index)
    (map-into result #'identity U_index)
    (loop :for index :from 2 :to c
       :do (progn (funcall *prf* P U_index U_index)
                  (loop :for i :from 0 :below *prf-output-length*
                     :do (setf (aref result i)
                               (logxor (aref result i)
                                       (aref U_index i))))))
    result))

(defun compute-kdf (password salt iteration-count intended-derived-key-length)
  (declare (optimize (speed 3) (space 0) (safety 3) (debug 0) (compilation-speed 0)))
  (let ((P (the (simple-array (unsigned-byte 8)) password))
        (S (the (simple-array (unsigned-byte 8)) salt))
        (c (the fixnum iteration-count))
        (dkLen (the fixnum intended-derived-key-length))
        (hLen *prf-output-length*))
    (if (> dkLen (the fixnum (* (the (integer 0 4294967296) (1- (expt 2 32)))
                                hLen)))
        (error "intended-derived-key-length too long"))
    (let ((l (ceiling dkLen hLen)))
      (apply #'concatenate '(vector (unsigned-byte 8))
             (loop :for index :from 1 :to (the fixnum l)
                :collect (F P S c index))))))

(defun compute-pbkdf2-hmac-sha256 (password salt iteration-count &optional (intended-derived-key-length 32))
  (declare (optimize (speed 3) (space 0) (safety 3) (debug 0) (compilation-speed 0)))
  (let ((*prf* #'com.hon.utils.crypto.hmac:compute-hmac-sha256)
        (*prf-output-length* 32))
    (compute-kdf password salt iteration-count intended-derived-key-length)))

;;; https://tools.ietf.org/html/rfc7914#page-12

(check-equals "PBKDF2-HMAC-SHA-256 (P=\"passwd\", S=\"salt\",c=1, dkLen=64)"
              (integer-to-bytes #x55ac046e56e3089fec1691c22544b605f94185216dde0465e68b9d57c20dacbc49ca9cccf179b645991664b39d77ef317c71b845b1e30bd509112041d3a19783
                                (* 64 8))
              (compute-pbkdf2-hmac-sha256 (map '(vector (unsigned-byte 8)) #'char-code "passwd")
                                          (map '(vector (unsigned-byte 8)) #'char-code "salt")
                                          1 64))

(check-equals "PBKDF2-HMAC-SHA-256 (P=\"Password\", S=\"NaCl\",c=80000, dkLen=64)"
              (integer-to-bytes #x4ddcd8f60b98be21830cee5ef22701f9641a4418d04c0414aeff08876b34ab56a1d425a1225833549adb841b51c9b3176a272bdebba1d078478f62b397f33c8d
                                (* 64 8))
              (compute-pbkdf2-hmac-sha256 (map '(vector (unsigned-byte 8)) #'char-code "Password")
                                          (map '(vector (unsigned-byte 8)) #'char-code "NaCl")
                                          80000 64))

(defun test (iterations)
  ;; (print (bytes-to-hex-string
  (compute-pbkdf2-hmac-sha256 (coerce (map 'vector #'char-code "Password")
                                      '(vector (unsigned-byte 8)))
                              (coerce (map 'vector #'char-code "NaCl")
                                      '(vector (unsigned-byte 8)))
                              iterations 64))
;; ))

;; (progn (sb-profile:profile com.hon.utils.crypto.sha256:compute-hash
;;                            com.hon.utils.crypto.hmac:compute-hmac-sha256
;;                            com.hon.utils.crypto.pbkdf2:compute-pbkdf2-hmac-sha256)
;;        (time (com.hon.utils.crypto.pbkdf2::test 3000))
;;        (sb-profile:report))
