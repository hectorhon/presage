(in-package :com.hon.templating.html)

(defvar *blocks* nil
  "An alist of block-name to block-contents.")

(defparameter *indent* "")

(defun escape-html (string &optional (attribute-p t))
  (with-output-to-string (out)
    (if attribute-p
        (loop :for char :across string
           :do (case char
                 (#\& (write-string "&amp;" out))
                 (#\< (write-string "&lt;" out))
                 (#\> (write-string "&gt;" out))
                 (#\" (write-string "&quot;" out))
                 (#\' (write-string "&#39;" out))
                 (t (write-char char out))))
        (loop :for char :across string
           :do (case char
                 (#\& (write-string "&amp;" out))
                 (#\< (write-string "&lt;" out))
                 (#\> (write-string "&gt;" out))
                 (t (write-char char out)))))))

(defun render (sexpr)
  (let ((tag-name (car sexpr)))
    (cond ((eq tag-name :extends)
           (let* ((parent-template-name (second sexpr))
                  (parent-template (read-template parent-template-name))
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
                :do (if (and (typep value 'list)
                             (eq :eval (first value)))
                        (format t " ~(~a~)=\"~a\"" name (escape-html (eval (second value))))
                        (format t " ~(~a~)=\"~a\"" name (escape-html value)))
                :finally (format t ">~%"))
             (let ((*indent* (concatenate 'string *indent* "  ")))
               (loop :for content :in contents
                  :do (cond ((typep content 'list)
                             (if (eq :eval (car content)) (format t "~a" *indent*))
                             (render content))
                            ((typep content 'symbol)
                             (format t "~a~a"
                                     *indent*
                                     (escape-html (format nil "~a" (symbol-value content)))))
                            (t
                             (format t "~a~a"
                                     *indent*
                                     (escape-html (format nil "~a" content)))))
                  :do (format t "~%")))
             (format t "~a</~(~a~)>" *indent* tag-name))))))


(defparameter *base-directory*
  (make-pathname :directory '(:relative "templating"))
  "The base directory containing the templates." )

(defparameter *templates*
  (make-hash-table :test 'equal))

(defvar *calling-package* nil
  "Special variable to store the package that read-template should READ into.")

(defun read-template (filename)
  "Read file contents as sexpr. The contents will be read into package
   *calling-package*."
  ;; We read into *calling-package* instead of taking a function argument such as
  ;; destination-package because the render function uses this function too.
  (with-open-file (stream (merge-pathnames filename *base-directory*))
    (let ((symbols (let ((original-package *package*))
                     ;; If *package* is not set correctly, symbol-value in render will
                     ;; not work because the symbols would be read into the active
                     ;; package (such as cl-user) instead of the package of the function
                     ;; calling render-template (which declares special lexical
                     ;; variables as a way to pass the context).
                     (prog2 (setf *package* *calling-package*)
                         (read stream)
                       (setf *package* original-package)))))
      (setf (gethash filename *templates*) symbols))))

(defmacro render-template (file-name)
  `(let ((*calling-package* ,*package*))
     (render (read-template ,file-name))))

(defun test ()
  (let ((*users* (list "user 9" "user 10")))
    (declare (special *users*))
    (render-template "page1.l")))
