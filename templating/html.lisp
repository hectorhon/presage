(in-package :com.hon.templating.html)

(defparameter *indent* "")

(defparameter *templates* nil)

(defvar *blocks* nil
  "An alist of block-name to block-contents.")

(defun render (sexpr)
  (let ((tag-name (car sexpr)))
    (cond ((eq tag-name :extends)
           (let* ((parent-template-name (second sexpr))
                  (parent-template (cdr (assoc parent-template-name *templates*)))
                  (*blocks* (mapcar (lambda (block-sexpr)
                                      (destructuring-bind (block-keyword block-name &rest block-contents) block-sexpr
                                        (declare (ignore block-keyword))
                                        (cons block-name block-contents)))
                                    (cddr sexpr))))
             (render parent-template)))
          ((eq tag-name :block)
           (let* ((block-name (second sexpr))
                  (child-sexprs (cdr (assoc block-name *blocks*))))
             (loop :for child-sexpr :in child-sexprs
                :do (render child-sexpr))))
          ((eq tag-name :eval)
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
           (destructuring-bind (attributes &rest contents) (cdr sexpr)
             (loop :initially (format t "~a<~(~a~)" *indent* tag-name)
                :for (name value) :in attributes
                :do (format t " ~(~a~)=\"~a\"" name value)
                :finally (format t ">~%"))
             (let ((*indent* (concatenate 'string *indent* "  ")))
               (loop :for content :in contents
                  :do (cond ((typep content 'list)
                             (if (eq :eval (car content)) (format t "~a" *indent*))
                             (render content))
                            ((typep content 'symbol)
                             (format t "~a~a" *indent* (symbol-value content)))
                            (t
                             (format t "~a~a" *indent* content)))
                  :do (format t "~%")))
             (format t "~a</~(~a~)>" *indent* tag-name))))))

(defparameter *template*
  '(:extends "layout.l"
    (:block content
      (ul ((class "sitemap")
           (id "asdf"))
          (div ((class "xxx")) "yyy")
          "zzz"
          (:for user :in *users*
                (li ()
                    (span () "here is ")
                    (a ((href "/link")) (:eval user))))))))

(defparameter *layout*
  '(body ()
    (:block content)))

(setf *templates* (acons "layout.l" *layout* *templates*))

(defun test ()
  (let ((*users* (list "user 9" "user 10")))
    (declare (special *users*))
    (render *template*)))
