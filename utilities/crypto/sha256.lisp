;;;; https://tools.ietf.org/html/rfc6234

(in-package :com.hon.utils.crypto.sha256)

(set-package-log-level nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-rotate-byte))

(declaim (inline mod32+ shr32 rotr32 rotl32))

(defmacro mod32++ (&rest forms)
  (case (length forms)
    (0 0)
    (1 (first forms))
    (2 `(mod32+ ,(first forms) ,(second forms)))
    (t `(mod32+ ,(first forms) (mod32++ ,@(cdr forms))))))

(defun mod32+ (a b)
  (declare (optimize (speed 3) (space 0) (safety 3) (debug 0) (compilation-speed 0)))
  (declare (type (unsigned-byte 32) a b))
  (ldb (byte 32 0) (+ a b)))

(defun shr32 (n x)
  (declare (optimize (speed 3) (space 0) (safety 3) (debug 0) (compilation-speed 0)))
  (declare (type (integer 0 32) n))
  (declare (type (unsigned-byte 32) x))
  (ash x (- n)))

(defun rotr32 (n x)
  (declare (optimize (speed 3) (space 0) (safety 3) (debug 0) (compilation-speed 0)))
  (declare (type (integer 1 31) n))
  (declare (type (unsigned-byte 32) x))
  (sb-rotate-byte:rotate-byte (- n) (byte 32 0) x))

(defun rotl32 (n x)
  (declare (optimize (speed 3) (space 0) (safety 3) (debug 0) (compilation-speed 0)))
  (declare (type (integer 1 31) n))
  (declare (type (unsigned-byte 32) x))
  (sb-rotate-byte:rotate-byte n (byte 32 0) x))

(declaim (inline ch maj bsig0 bsig1 ssig0 ssig1))

(defun ch (x y z)
  (declare (optimize (speed 3) (space 0) (safety 3) (debug 0) (compilation-speed 0)))
  (declare (type (unsigned-byte 32) x y z))
  (logxor (logand x y)
          (logand (ldb (byte 32 0) (lognot x)) z)))

(defun maj (x y z)
  (declare (optimize (speed 3) (space 0) (safety 3) (debug 0) (compilation-speed 0)))
  (declare (type (unsigned-byte 32) x y z))
  (logxor (logand x y)
          (logand x z)
          (logand y z)))

(defun bsig0 (x)
  (declare (optimize (speed 3) (space 0) (safety 3) (debug 0) (compilation-speed 0)))
  (declare (type (unsigned-byte 32) x))
  (logxor (rotr32 2 x)
          (rotr32 13 x)
          (rotr32 22 x)))

(defun bsig1 (x)
  (declare (optimize (speed 3) (space 0) (safety 3) (debug 0) (compilation-speed 0)))
  (declare (type (unsigned-byte 32) x))
  (logxor (rotr32 6 x)
          (rotr32 11 x)
          (rotr32 25 x)))

(defun ssig0 (x)
  (declare (optimize (speed 3) (space 0) (safety 3) (debug 0) (compilation-speed 0)))
  (declare (type (unsigned-byte 32) x))
  (logxor (rotr32 7 x)
          (rotr32 18 x)
          (shr32 3 x)))

(defun ssig1 (x)
  (declare (optimize (speed 3) (space 0) (safety 3) (debug 0) (compilation-speed 0)))
  (declare (type (unsigned-byte 32) x))
  (logxor (rotr32 17 x)
          (rotr32 19 x)
          (shr32 10 x)))

(defconstant +constants+
  (if (boundp '+constants+)
      (symbol-value '+constants+)
      (make-array 64
                  :element-type '(unsigned-byte 32)
                  :initial-contents
                  #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
                    #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
                    #xd807aa98 #x12835b01 #x243185be #x550c7dc3
                    #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
                    #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
                    #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
                    #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
                    #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
                    #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
                    #x650a7354 #x766a0abb #x81c2c92e #x92722c85
                    #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
                    #xd192e819 #xd6990624 #xf40e3585 #x106aa070
                    #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
                    #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
                    #x748f82ee #x78a5636f #x84c87814 #x8cc70208
                    #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))))

(defconstant +initial-hash-value+
  (if (boundp '+initial-hash-value+)
      (symbol-value '+initial-hash-value+)
      (make-array 8
                  :element-type '(unsigned-byte 32)
                  :initial-contents
                  #(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                    #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))))

(let ((message-schedule  (make-array 64 :element-type '(unsigned-byte 32)))
      (working-variables (make-array 8  :element-type '(unsigned-byte 32)))
      (hash-value        (make-array 8  :element-type '(unsigned-byte 32)))
      (message-block     (make-array 64 :element-type '(unsigned-byte 8)))
      (result            (make-array 32 :element-type '(unsigned-byte 8))))
  (macrolet ((a () '(aref hash-value 0)) (b () '(aref hash-value 1))
             (c () '(aref hash-value 2)) (d () '(aref hash-value 3))
             (e () '(aref hash-value 4)) (f () '(aref hash-value 5))
             (g () '(aref hash-value 6)) (h () '(aref hash-value 7)))
    (defun compute-hash (message-bytes)
      (declare (optimize (speed 3) (space 0) (safety 3) (debug 0) (compilation-speed 0)))
      (declare (type (simple-array (unsigned-byte 8)) message-bytes))
      (map-into hash-value #'identity +initial-hash-value+) ; Initialize the hash-value
      (let ((original-message-length (length message-bytes))
            (message-bytes-index 0)
            (1-appended-p nil))
        (declare (type (unsigned-byte 59) message-bytes-index)
                 (type (unsigned-byte 59) original-message-length))
        (flet ((get-next-message-block () ; returns t if it should be called again
                 (if 1-appended-p
                     (progn (loop :for destination-index :from 0 :below 56
                               :do (setf (aref message-block destination-index) #x00))
                            (loop :for destination-index :from 56
                               :for byte :across (integer-64-to-bytes (* 8 original-message-length))
                               :do (setf (aref message-block destination-index) byte))
                            nil)
                     (let ((destination-index 0))
                       (progn (loop :while (< destination-index 64)
                                 :for source-index :from message-bytes-index :below original-message-length
                                 :do (setf (aref message-block destination-index)
                                           (aref message-bytes source-index))
                                 :do (incf destination-index))
                              (incf message-bytes-index 64))
                       (if (< destination-index 64)
                           (progn (setf (aref message-block destination-index) #x80)
                                  (setf 1-appended-p t)
                                  (incf destination-index)
                                  (if (>= destination-index 56)
                                      (progn (loop :until (eql 64 destination-index)
                                                :do (setf (aref message-block destination-index) #x00)
                                                :do (incf destination-index))
                                             t)
                                      (progn (loop :until (eql 56 destination-index)
                                                :do (setf (aref message-block destination-index) #x00)
                                                :do (incf destination-index))
                                             (loop :for byte :across (integer-64-to-bytes (* 8 original-message-length))
                                                :do (setf (aref message-block destination-index) byte)
                                                :do (incf destination-index))
                                             nil)))
                           t))))
               (prepare-message-schedule ()
                 (loop :for tt :from 0 :to 15
                    :do (setf (aref message-schedule tt) 0))
                 (loop :for byte :across message-block
                    :for byte-index :of-type (integer 0 64) :from 0
                    :do (multiple-value-bind (tt pos) (floor byte-index 4)
                          (incf (aref message-schedule tt)
                                (ash byte (* 8 (- 3 pos))))))
                 (loop :for tt :from 16 :to 63
                    :do (setf (aref message-schedule tt)
                              (mod32++ (ssig1 (aref message-schedule (- tt 2)))
                                       (aref message-schedule (- tt 7))
                                       (ssig0 (aref message-schedule (- tt 15)))
                                       (aref message-schedule (- tt 16))))))
               (initialize-working-variables ()
                 (map-into working-variables #'identity hash-value))
               (perform-main-hash-computation ()
                 (loop :for tt :from 0 :to 63
                    :do (let ((t1 (mod32++ (h)
                                           (bsig1 (e))
                                           (ch (e) (f) (g))
                                           (aref +constants+ tt)
                                           (aref message-schedule tt)))
                              (t2 (mod32++ (bsig0 (a))
                                           (maj (a) (b) (c)))))
                          (setf (h) (g))
                          (setf (g) (f))
                          (setf (f) (e))
                          (setf (e) (mod32+ (d) t1))
                          (setf (d) (c))
                          (setf (c) (b))
                          (setf (b) (a))
                          (setf (a) (mod32+ t1 t2)))))
               (compute-intermediate-hash-value ()
                 (loop :for i :from 0 :below 8
                    :do (setf (aref hash-value i)
                              (mod32+ (aref working-variables i)
                                      (aref hash-value i))))))
          (declare (inline prepare-message-schedule
                           initialize-working-variables
                           perform-main-hash-computation
                           compute-intermediate-hash-value))
          (loop :for has-next-block-p = (get-next-message-block)
             :do (progn (prepare-message-schedule)
                        (initialize-working-variables)
                        (perform-main-hash-computation)
                        (compute-intermediate-hash-value))
             :until (not has-next-block-p))
          (loop :for hash-value-part :across hash-value
             :for i :of-type (integer 0 7) :from 0
             :do (loop :for j :from 0 :below 4
                    :do (setf (aref result (+ j (* i 4)))
                              (ldb (byte 8 (* 8 (- 3 j))) hash-value-part))))
          result)))))

(check-equals "SHA256 Hash for one block message sample"
              (integer-to-bytes #xBA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD (* 32 8))
              (compute-hash (coerce (map 'vector #'char-code "abc")
                                    '(vector (unsigned-byte 8)))))

(check-equals "SHA256 Hash for two block message sample"
              (integer-to-bytes #x248D6A61D20638B8E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1 (* 32 8))
              (compute-hash (coerce (map 'vector #'char-code "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
                                    '(vector (unsigned-byte 8)))))

;; (progn (sb-profile:profile com.hon.utils.crypto.sha256:compute-hash
;;                                     com.hon.utils.crypto.sha256::mod32+
;;                                     com.hon.utils.crypto.sha256::shr32
;;                                     com.hon.utils.crypto.sha256::rotr32
;;                                     com.hon.utils.crypto.sha256::rotl32
;;                                     com.hon.utils.crypto.sha256::pad-message
;;                                     com.hon.utils.crypto.sha256::ch
;;                                     com.hon.utils.crypto.sha256::maj
;;                                     com.hon.utils.crypto.sha256::bsig0
;;                                     com.hon.utils.crypto.sha256::ssig0
;;                                     com.hon.utils.crypto.sha256::ssig1
;;                                     com.hon.utils.byte:integer-32-to-bytes
;;                                     com.hon.utils.byte:integer-64-to-bytes
;;                                     com.hon.utils.byte:bytes-to-integer-32
;;                                     ;; com.hon.utils.crypto.hmac::compute-mac
;;                                     ;; com.hon.utils.crypto.pbkdf2::compute-kdf
;;                                     )
;;                 (time (com.hon.utils.crypto.pbkdf2::test 80000))
;;                 (sb-profile:report))
