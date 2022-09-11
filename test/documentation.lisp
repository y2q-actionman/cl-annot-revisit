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

(test test-@doc-defun-setf
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation "hoge"
         #1=(defun (setf foo) (new-value x)))
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
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation "doc"
         #1=(lambda (x y z) (+ x y z)))
       '(let ((#:obj #1#))
         (setf (documentation #:obj 'function) "doc")
         #:obj))))

(test test-@doc-lambda-func
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation "doc"
         #1=#'(lambda (x y z) (+ x y z)))
       '(let ((#:obj #1#))
         (setf (documentation #:obj 'function) "doc")
         #:obj)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation "doc"
         #2=#'(lambda ()))
       '(let ((#:obj #2#))
         (setf (documentation #:obj 'function) "doc")
         #:obj))))
