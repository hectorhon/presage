(in-package :com.hon.utils.test)

(defmacro check-equals (description expected actual)
  "Checks that the arguments are #'equal."
  `(funcall (lambda (expected actual)
              (progn (format t "TEST ~a..." ,description)
                     (cond ((and (typep expected 'vector)
                                 (typep actual 'vector))
                            (if (eql (length expected) (length actual))
                                (loop :for i :from 0 :below (length actual)
                                   :for actual-elem = (aref actual i)
                                   :and expected-elem = (aref expected i)
                                   :unless (equal actual-elem expected-elem)
                                   :do (progn (format t "Failed. Expected ~a but got ~a instead (index ~d).~%"
                                                      expected actual i)
                                              (error "Expected ~a but got ~a instead (index ~d)."
                                                     expected actual i)))
                                (format t "OK.~%")))
                           ((equal expected actual)
                            (format t " OK.~%"))
                           (t
                            (format t " Failed. Expected ~a but got ~a instead.~%"
                                    expected actual)
                            (error "Expected ~a but got ~a instead."
                                   expected actual)))))
            ,expected ,actual))
