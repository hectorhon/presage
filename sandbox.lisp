(defparameter *template*
  (quote (ul ((class "sitemap")
              (id "asdf"))
             (:for user :in *users*
                   (li ()
                       (a ((href "/link")) (:eval user)))))))

(defparameter *indent* "")

(defun render (sexpr)
  (let ((tag-name (car sexpr)))
    (cond ((eq tag-name :eval)
           (let ((eval-result (eval (second sexpr))))
             (format t "~a" eval-result)))
          ((eq tag-name :for)
           (destructuring-bind (for-keyword variable-name in-keyword collection-name inner-sexpr) sexpr
             (declare (ignore for-keyword in-keyword))
             (loop :with items = (symbol-value collection-name)
                :with items-count = (length items)
                :for item :in items
                :for item-counter :upfrom 1
                :do (progv (list variable-name) (list item)
                      (render inner-sexpr))
                :unless (eql items-count item-counter) ; add newline, except for last item
                :do (format t "~%"))))
          (t
           (destructuring-bind (attributes contents) (cdr sexpr)
             (format t "~a" *indent*)
             (loop :initially (format t "<~(~a~)" tag-name)
                :for (name value) :in attributes
                :do (format t " ~(~a~)=\"~a\"" name value)
                :finally (format t ">"))
             (cond ((typep contents 'list)
                    (let ((*indent* (concatenate 'string *indent* "  ")))
                      (format t "~%")
                      (if (eq :eval (car contents)) (format t "~a" *indent*))
                      (render contents)
                      (format t "~%"))
                    (format t "~a" *indent*)) ; indent for closing tag
                   ((typep contents 'symbol)
                    (format t "~a" (symbol-value contents)))
                   (t
                    (format t "~a" contents)))
             (format t "</~(~a~)>" tag-name))))))

(defun test ()
  (let ((*users* (list "user 9" "user 10")))
    (declare (special *users*))
    (render *template*)))
