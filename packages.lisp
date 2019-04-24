(defpackage :com.hon.test-utils
  (:use :common-lisp)
  (:export :check-equals))

(defpackage :com.hon.string-utils
  (:use :common-lisp)
  (:use :com.hon.test-utils))

(defpackage :com.hon.http
  (:use :common-lisp)
  (:use :com.hon.test-utils))

(defpackage :com.hon.server
  (:use :sb-bsd-sockets)
  (:use :common-lisp))
