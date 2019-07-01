;;;; https://tools.ietf.org/html/rfc6234

(in-package :com.hon.utils.crypto.sha256)

(set-package-log-level nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-rotate-byte))

(defun mod32+ (&rest xs)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (reduce (lambda (a b)
            (declare (type (unsigned-byte 32) a b))
            (ldb (byte 32 0) (+ a b)))
          xs))

(defun shr32 (n x)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type (integer 0 32) n))
  (declare (type (unsigned-byte 32) x))
  (ash x (- n)))

(defun rotr32 (n x)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type fixnum n))
  (declare (type (unsigned-byte 32) x))
  (sb-rotate-byte:rotate-byte
   (the (integer 0 31) (- 32 n))
   (byte 32 0) x))

(defun rotl32 (n x)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type fixnum n))
  (declare (type (unsigned-byte 32) x))
  (sb-rotate-byte:rotate-byte
   (the (integer -31 0) n)
   (byte 32 0) x))

(defun pad-message (bytes)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type vector bytes))
  (let* ((original-length (length bytes))
         (final-length (* 64 (ceiling (+ original-length
                                         1  ; the #x80 byte
                                         8) ; the message length at the end
                                      64))))
    (the (integer 0 576460752303423487) original-length)
    (the fixnum final-length)
    (adjust-array bytes final-length :fill-pointer original-length)
    (vector-push #b10000000 bytes)
    (loop :for i :from 0 :below (- final-length original-length 1 8)
       :do (vector-push 0 bytes))
    (loop :for byte
       :across (the (simple-array (unsigned-byte 8))
                    (integer-to-bytes (the fixnum (* 8 original-length)) 64))
       :do (vector-push byte bytes))
    bytes))

(defun ch (x y z)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type (unsigned-byte 32) x y z))
  (logxor (logand x y)
          (logand (lognot x) z)))

(defun maj (x y z)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type (unsigned-byte 32) x y z))
  (logxor (logand x y)
          (logand x z)
          (logand y z)))

(defun bsig0 (x)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (logxor (rotr32 2 x)
          (rotr32 13 x)
          (rotr32 22 x)))

(defun bsig1 (x)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (logxor (rotr32 6 x)
          (rotr32 11 x)
          (rotr32 25 x)))

(defun ssig0 (x)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (logxor (rotr32 7 x)
          (rotr32 18 x)
          (shr32 3 x)))

(defun ssig1 (x)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (logxor (rotr32 17 x)
          (rotr32 19 x)
          (shr32 10 x)))

(defparameter constants
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
    #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

(defun compute-hash (bytes)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type (vector (unsigned-byte 8)) bytes))
  (declare (type (simple-vector 64) constants))
  (let* ((bytes (pad-message (make-array (length bytes)
                                         :adjustable t
                                         :fill-pointer t
                                         :initial-contents bytes)))
         (num-blocks (/ (length bytes) 64))
         (hash-value (make-array 8 :initial-contents
                                 #(#x6a09e667
                                   #xbb67ae85
                                   #x3c6ef372
                                   #xa54ff53a
                                   #x510e527f
                                   #x9b05688c
                                   #x1f83d9ab
                                   #x5be0cd19))))
    (loop :initially (log-debug "Initial hash value:")
       :for value :across hash-value
       :for index fixnum :upfrom 0
       :do (log-debug "H[~d] = ~8,'0x" index value)
       :finally (log-debug ""))
    (loop :for i fixnum :from 1 :to num-blocks
       :do (let ((message-block (subseq bytes (* (1- i) 64) (* i 64)))
                 (message-schedule (make-array 64))
                 (a) (b) (c) (d) (e) (f) (g) (h) (t1) (t2))
             (loop :initially (log-debug "Block contents:")
                :for i :from 0 :below 64 :by 4
                :for n fixnum :from 0
                :do (log-debug "W[~d] = ~2,'0x~2,'0x~2,'0x~2,'0x" n
                               (aref message-block (+ i 0))
                               (aref message-block (+ i 1))
                               (aref message-block (+ i 2))
                               (aref message-block (+ i 3)))
                :finally (log-debug ""))
             ;; 1. Prepare the message schedule
             (loop :for tt :from 0 :to 15
                :do (let ((rhs (subseq message-block (* 4 tt) (* 4 (1+ tt)))))
                      (setf (aref message-schedule tt)
                            (bytes-to-integer rhs))))
             (loop :for tt :from 16 :to 63
                :do (let ((rhs (mod32+ (ssig1 (aref message-schedule (- tt 2)))
                                       (aref message-schedule (- tt 7))
                                       (ssig0 (aref message-schedule (- tt 15)))
                                       (aref message-schedule (- tt 16)))))
                      (setf (aref message-schedule tt) rhs)))
             ;; 2. Initialize the working variables
             (setf a (aref hash-value 0))
             (setf b (aref hash-value 1))
             (setf c (aref hash-value 2))
             (setf d (aref hash-value 3))
             (setf e (aref hash-value 4))
             (setf f (aref hash-value 5))
             (setf g (aref hash-value 6))
             (setf h (aref hash-value 7))
             ;; 3. Perform the main hash computation
             (loop :for tt :from 0 :to 63
                :do (progn (setf t1 (mod32+ h
                                            (bsig1 e)
                                            (ch e f g)
                                            (aref constants tt)
                                            (aref message-schedule tt)))
                           (setf t2 (mod32+ (bsig0 a)
                                            (maj a b c)))
                           (setf h g)
                           (setf g f)
                           (setf f e)
                           (setf e (mod32+ d t1))
                           (setf d c)
                           (setf c b)
                           (setf b a)
                           (setf a (mod32+ t1 t2))
                           (log-debug "~d: ~8,'0x ~8,'0x ~8,'0x ~8,'0x ~8,'0x ~8,'0x ~8,'0x ~8,'0x"
                                      tt a b c d e f g h)))
             (log-debug "")
             ;; 4. Compute the intermediate hash value
             (setf (aref hash-value 0) (mod32+ a (aref hash-value 0)))
             (setf (aref hash-value 1) (mod32+ b (aref hash-value 1)))
             (setf (aref hash-value 2) (mod32+ c (aref hash-value 2)))
             (setf (aref hash-value 3) (mod32+ d (aref hash-value 3)))
             (setf (aref hash-value 4) (mod32+ e (aref hash-value 4)))
             (setf (aref hash-value 5) (mod32+ f (aref hash-value 5)))
             (setf (aref hash-value 6) (mod32+ g (aref hash-value 6)))
             (setf (aref hash-value 7) (mod32+ h (aref hash-value 7))))
       :finally (return (concatenate 'vector
                                     (integer-to-bytes (aref hash-value 0) 32)
                                     (integer-to-bytes (aref hash-value 1) 32)
                                     (integer-to-bytes (aref hash-value 2) 32)
                                     (integer-to-bytes (aref hash-value 3) 32)
                                     (integer-to-bytes (aref hash-value 4) 32)
                                     (integer-to-bytes (aref hash-value 5) 32)
                                     (integer-to-bytes (aref hash-value 6) 32)
                                     (integer-to-bytes (aref hash-value 7) 32))))))

(check-equals "SHA256 Hash for one block message sample"
              (integer-to-bytes #xBA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD (* 32 8))
              (compute-hash (coerce (map 'vector #'char-code "abc")
                                    '(vector (unsigned-byte 8)))))

(check-equals "SHA256 Hash for two block message sample"
              (integer-to-bytes #x248D6A61D20638B8E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1 (* 32 8))
              (compute-hash (coerce (map 'vector #'char-code "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
                                    '(vector (unsigned-byte 8)))))
