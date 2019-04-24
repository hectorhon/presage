(defpackage :com.hon.test-utils
  (:use :common-lisp)
  (:export :check-equals))

(defpackage :com.hon.string-utils
  (:use :common-lisp
        :com.hon.test-utils)
  (:export :split-string))

(defpackage :com.hon.http
  (:use :common-lisp
        :com.hon.test-utils
        :com.hon.string-utils))

(defpackage :com.hon.server
  (:use :sb-bsd-sockets)
  (:use :common-lisp))
