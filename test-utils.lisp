(in-package :com.hon.test-utils)

(defmacro check-equals (description expected actual)
  "Checks that the list contents are #'equal."
  `(funcall (lambda (expected actual)
              (progn (format t "TEST ~a..." ,description)
                     (if (equal expected actual)
                         (format t " OK.~%")
                         (progn (format t " Failed.~%")
                                (error "Expected ~a but got ~a instead."
                                       expected actual)))))
            ,expected ,actual))
