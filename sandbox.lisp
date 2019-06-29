(require :sb-rotate-byte)

(defun mod32+ (&rest xs)
  (reduce (lambda (a b) (ldb (byte 32 0) (+ a b))) xs))

(defun shr32 (n x)
  (ash x (- n)))

(defun rotr32 (n x)
  (sb-rotate-byte:rotate-byte (- 32 n) (byte 32 0) x))

(defun rotl32 (n x)
  (sb-rotate-byte:rotate-byte n (byte 32 0) x))

(defun pad-message (bytes)
  (let* ((original-length (length bytes))
         (final-length (* 64 (ceiling (+ original-length
                                         1  ; the #x80 byte
                                         8) ; the message length at the end
                                      64))))
    (adjust-array bytes final-length :fill-pointer original-length)
    (vector-push #b10000000 bytes)
    (loop :for i :from 0 :below (- final-length original-length 1 8)
       :do (vector-push 0 bytes))
    (loop :for byte
       :across (com.hon.utils.byte:integer-to-bytes
                (* 8 original-length)
                64)
       :do (vector-push byte bytes))
    bytes))

(defun ch (x y z)
  (logxor (logand x y)
          (logand (lognot x) z)))

(defun maj (x y z)
  (logxor (logand x y)
          (logand x z)
          (logand y z)))

(defun bsig0 (x)
  (logxor (rotr32 2 x)
          (rotr32 13 x)
          (rotr32 22 x)))

(defun bsig1 (x)
  (logxor (rotr32 6 x)
          (rotr32 11 x)
          (rotr32 25 x)))

(defun ssig0 (x)
  (logxor (rotr32 7 x)
          (rotr32 18 x)
          (shr32 3 x)))

(defun ssig1 (x)
  (logxor (rotr32 17 x)
          (rotr32 19 x)
          (shr32 10 x)))

(defvar constants
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
  (let* ((bytes (pad-message bytes))
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
    (loop :initially (format t "Initial hash value: ~%")
       :for value :across hash-value
       :for index :upfrom 0
       :do (format t "H[~d] = ~8,'0x~%" index value)
       :finally (format t "~%"))
    (loop :for i :from 1 :to num-blocks
       :do (let ((message-block (subseq bytes (* (1- i) 64) (* i 64)))
                 (message-schedule (make-array 64))
                 (a) (b) (c) (d) (e) (f) (g) (h) (t1) (t2))
             (loop :initially (format t "Block contents: ~%")
                :for i :from 0 :below 64 :by 4
                :for n :from 0
                :do (format t "W[~d] = ~2,'0x~2,'0x~2,'0x~2,'0x~%" n
                            (aref message-block (+ i 0))
                            (aref message-block (+ i 1))
                            (aref message-block (+ i 2))
                            (aref message-block (+ i 3)))
                :finally (format t "~%"))
             ;; 1. Prepare the message schedule
             (loop :for tt :from 0 :to 15
                :do (let ((rhs (subseq message-block (* 4 tt) (* 4 (1+ tt)))))
                      (setf (aref message-schedule tt)
                            (com.hon.utils.byte:bytes-to-integer rhs))))
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
                           (format t "~d: ~8,'0x ~8,'0x ~8,'0x ~8,'0x ~8,'0x ~8,'0x ~8,'0x ~8,'0x~%"
                                   tt a b c d e f g h)))
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
                                     (com.hon.utils.byte:integer-to-bytes (aref hash-value 0) 32)
                                     (com.hon.utils.byte:integer-to-bytes (aref hash-value 1) 32)
                                     (com.hon.utils.byte:integer-to-bytes (aref hash-value 2) 32)
                                     (com.hon.utils.byte:integer-to-bytes (aref hash-value 3) 32)
                                     (com.hon.utils.byte:integer-to-bytes (aref hash-value 4) 32)
                                     (com.hon.utils.byte:integer-to-bytes (aref hash-value 5) 32)
                                     (com.hon.utils.byte:integer-to-bytes (aref hash-value 6) 32)
                                     (com.hon.utils.byte:integer-to-bytes (aref hash-value 7) 32))))))

(defun test ()
  (let ((arr (make-array 3
                         :initial-contents #(#x61 #x62 #x63)
                         :fill-pointer t
                         :adjustable t)))
    (loop :for byte :across (compute-hash arr)
       :do (format t "~2,'0x" byte))))

(defun test2 ()
  (let* ((str "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
         (arr (make-array 56
                          :initial-contents (map 'vector #'char-code str)
                          :fill-pointer t
                          :adjustable t)))
    (loop :for byte :across (compute-hash arr)
       :do (format t "~2,'0x" byte))))
