(in-package #:cl-annot-revisit-test)

(test test-@doc-defclass
  (is (equal-ignoring-gensym
       (macroexpand-1 '(cl-annot-revisit:documentation #1="hoge"
                        #2=(defclass x () ())))
       '(let ((#:obj #2#))
         (setf (documentation #:obj 'type) #1#)
         #:obj) ))
  (is (equal-ignoring-gensym
       (macroexpand-1 '(cl-annot-revisit:documentation #3="fuga"
                        #4=(defclass y (super1) (slot1 slot2) (:metaclass meta))))
       '(let ((#:obj #4#))
         (setf (documentation #:obj 'type) #3#)
         #:obj))))

(test test-@doc-lambda
  ;; This is checking `parse-body' usage.
  (is (equal
       (macroexpand-1 '(cl-annot-revisit:documentation "doc" (lambda (x y z) (+ x y z))))
       '(lambda (x y z) "doc" (+ x y z))))
  (is (equal
       (macroexpand-1 '(cl-annot-revisit:documentation "doc" (lambda ()))) ; This is a bug of cl-annot.
       '(lambda () "doc" nil)))
  (is (equal
       (macroexpand-1 '(cl-annot-revisit:documentation "doc" (lambda () nil)))
       '(lambda () "doc" nil)))
  (is (equal
       (macroexpand-1 '(cl-annot-revisit:documentation "doc" (lambda () #1="this is not docstring")))
       '(lambda () "doc" #1#)))
  (is (equal
       (macroexpand-1 '(cl-annot-revisit:documentation "doc" (lambda (x) (declare (ignore x)))))
       '(lambda (x) (declare (ignore x)) "doc" nil)))
  (is (equal
       (macroexpand-1 '(cl-annot-revisit:documentation "doc" (lambda (x) (declare (ignore x)) 100)))
       '(lambda (x) (declare (ignore x)) "doc" 100)))
  t)

(test test-@doc-lambda-func
  (is (equal
       (macroexpand-1 '(cl-annot-revisit:documentation "doc" #'(lambda (x y z) (+ x y z))))
       '#'(lambda (x y z) "doc" (+ x y z))))
  (is (equal
       (macroexpand-1 '(cl-annot-revisit:documentation "doc" #'(lambda ())))
       '#'(lambda () "doc" nil)))
  t)
