(in-package #:cl-annot-revisit-test)

(test test-@doc-alias
  (is (equal
       (macroexpand-1 '(cl-annot-revisit:doc "hoge"))
       '(cl-annot-revisit:documentation "hoge") ))
  (is (equal
       (macroexpand-1 '(cl-annot-revisit:doc "fuga" (defun x ())))
       '(cl-annot-revisit:documentation "fuga" (defun x ()))))
  (is (equal
       (macroexpand-1 '(cl-annot-revisit:doc "piyo" (list x) (list y)))
       '(cl-annot-revisit:documentation "piyo" (list x) (list y)))))

(test test-@doc-empty
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation "hoge")
       nil)))

(test test-@doc-defun
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation "hoge"
         #1=(defun foo (x)))
       '(let ((#:obj #1#))
         (setf (documentation #:obj 'function) "hoge")
         #:obj))))

(test test-@doc-defclass
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation #1="hoge"
         #2=(defclass x () ()))
       '(let ((#:obj #2#))
         (setf (documentation #:obj 'type) #1#)
         #:obj) ))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation #3="fuga"
         #4=(defclass y (super1) (slot1 slot2) (:metaclass meta)))
       '(let ((#:obj #4#))
         (setf (documentation #:obj 'type) #3#)
         #:obj))))

(test test-@doc-lambda
  ;; This is checking `parse-body' usage.
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation "doc"
         (lambda (x y z) (+ x y z)))
       '(lambda (x y z) "doc" (+ x y z))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation "doc"
         (lambda ())) ; This is a bug of cl-annot.
       '(lambda () "doc" nil)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation "doc"
         (lambda () nil))
       '(lambda () "doc" nil)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation "doc"
         (lambda () #1="this is not docstring"))
       '(lambda () "doc" #1#)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation "doc"
         (lambda (x) (declare (ignore x))))
       '(lambda (x) (declare (ignore x)) "doc" nil)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation "doc"
         (lambda (x) (declare (ignore x)) 100))
       '(lambda (x) (declare (ignore x)) "doc" 100)))
  t)

(test test-@doc-lambda-func
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation "doc"
         #'(lambda (x y z) (+ x y z)))
       '#'(lambda (x y z) "doc" (+ x y z))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation "doc"
         #'(lambda ()))
       '#'(lambda () "doc" nil)))
  t)
