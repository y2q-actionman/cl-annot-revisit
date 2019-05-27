(in-package #:cl-annot-revisit-test)

(test test-@doc-defclass
  (is (equal
       (macroexpand-1 '(@doc "hoge" (defclass x () ())))
       '(defclass x () () (:documentation "hoge"))))
  (is (equal
       (macroexpand-1 '(@doc "fuga" (defclass y (super1) (slot1 slot2) (:metaclass meta))))
       '(defclass y (super1) (slot1 slot2) (:documentation "fuga") (:metaclass meta)))))
