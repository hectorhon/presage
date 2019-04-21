(in-package :com.hon.test-utils)

(defmacro check-equals (expected &body actual)
  "Simple test."
  `(let ((actual ,@actual))
     (if (equal ,expected actual)
         (format t "Test OK.")
         (error "Expected ~a but got ~a instead." ,expected actual))))

(check-equals 1 1)
