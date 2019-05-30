(in-package :cl-annot-revisit-test)

(test test-@metaclass
  (is (equal (macroexpand-1 '(@metaclass bar
                              (defclass foo ()
                                ())))
             '(defclass foo ()
               ()
               (:metaclass bar)))))

(test test-@export-slots
  (is (equal (macroexpand-1 '(@export-slots
                              #1=(define-condition hoge () #2=(slot1 slot2))))
             '(progn
               (eval-when #.+at-macro-eval-always+ (export '#2#))
               #1#))))

(test test-@export-accessors
  (is (equal (macroexpand-1 '(@export-accessors
                              #3=(defclass foo ()
                                   (slot0
                                    (slot1)
                                    (slot2 :reader x :reader y :accessor z :writer (setf p))))))
             '(progn
               (eval-when #.+at-macro-eval-always+ (export '(x y z p)))
               #3#))))

(test test-@export-class
  (is (equal (macroexpand-1 '(@export-class
                              #3=(defclass foo () ())))
             '(@export-slots (@export-accessors (@export #3#))))))



(test test-@export-accessors-defstruct
  (is (equal (macroexpand-1 '(@export-accessors
                              #1=(defstruct foo slot1 slot2)))
             '(progn (eval-when #.+at-macro-eval-always+ (export '(foo-slot1 foo-slot2)))
               #1#))))
