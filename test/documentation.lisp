(in-package #:cl-annot-revisit-test)

(test test-@doc-defclass
  (is (equal-ignoring-gensym
       (macroexpand-1 '(@documentation #1="hoge"
                        #2=(defclass x () ())))
       '(let ((#:obj #2#))
         (setf (documentation #:obj 'type) #1#)
         #:obj) ))
  (is (equal-ignoring-gensym
       (macroexpand-1 '(@documentation #3="fuga"
                        #4=(defclass y (super1) (slot1 slot2) (:metaclass meta))))
       '(let ((#:obj #4#))
         (setf (documentation #:obj 'type) #3#)
         #:obj))))
