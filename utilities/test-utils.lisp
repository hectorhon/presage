(in-package :com.hon.utils.test)

(defmacro check-equals (description expected actual)
  "Checks that the arguments are #'equal."
  `(funcall (lambda (expected actual)
              (progn (format t "TEST ~a..." ,description)
                     (if (equal expected actual)
                         (format t " OK.~%")
                         (progn (format t " Failed. Expected ~a but got ~a instead.~%"
                                        expected actual)
                                (error "Expected ~a but got ~a instead."
                                       expected actual)))))
            ,expected ,actual))
