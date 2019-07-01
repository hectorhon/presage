(defpackage :com.hon.utils.test
  (:use :common-lisp)
  (:export check-equals))

(defpackage :com.hon.utils.string
  (:use :common-lisp
        :com.hon.utils.test)
  (:export replace-all
           split-string
           assoc-value))

(defpackage :com.hon.utils.logging
  (:use :common-lisp
        :com.hon.utils.string)
  (:export set-package-log-level
           log-debug))

(defpackage :com.hon.utils.byte
  (:use :common-lisp
        :com.hon.utils.test)
  (:export integer-to-bytes
           bytes-to-integer
           bytes-to-hex-string))

(defpackage :com.hon.utils.crypto.sha256
  (:use :common-lisp
        :com.hon.utils.logging
        :com.hon.utils.test
        :com.hon.utils.byte)
  (:export compute-hash))

(defpackage :com.hon.utils.crypto.hmac
  (:use :common-lisp
        :com.hon.utils.logging
        :com.hon.utils.test
        :com.hon.utils.byte)
  (:export compute-hmac-sha256))

(defpackage :com.hon.utils.crypto.pbkdf2
  (:use :common-lisp
        :com.hon.utils.logging
        :com.hon.utils.test
        :com.hon.utils.byte)
  (:export compute-pbkdf2-hmac-sha256))
