(defpackage :com.hon.test-utils
  (:use :common-lisp)
  (:export :check-equals))

(defpackage :com.hon.string-utils
  (:use :common-lisp
        :com.hon.test-utils)
  (:export :split-string
           :assoc-value))

(defpackage :com.hon.http
  (:use :common-lisp
        :sb-bsd-sockets
        :com.hon.test-utils
        :com.hon.string-utils))
