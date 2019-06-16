(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        (lambda (&rest args)
          (reduce #'funcall fns
                  :from-end t
                  :initial-value (apply fn1 args))))
      #'identity))

(defun lists-equal-p (&rest lists)
  (let ((list1 (car lists))
        (lists (cdr lists)))
    (if list1
        (if lists
            (if (equal list1 (car lists))
                (apply #'lists-equal-p lists))
            t)
        t)))
