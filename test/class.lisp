(in-package :cl-annot-revisit-test)

(test test-metaclass
  (is (equal-after-macroexpand
       '(cl-annot-revisit:metaclass bar
         (defclass foo ()
           ()))
       '(defclass foo ()
         ()
         (:metaclass bar))))
  (signals at-macro-error
    (macroexpand-1 '(cl-annot-revisit:metaclass bar
                     (defclass foo ()
                       ()
                       (:documentation "doc")
                       (:metaclass baz)))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:metaclass bar
         #1=(defun foo () ()))
       '#1#)))

(test test-export-slots
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export-slots
         #1=(define-condition hoge () #2=(slot1 slot2)))
       '(progn
         (eval-when (:compile-toplevel :load-toplevel :execute) (export '#2#))
         #1#)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export-slots
         #3=(defclass hoge (standard-object) ()))
       '#3#))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:export-slots
         #4=(defun foo () ())
         #5=(list x y z))
       '(progn #4# #5#))))

(test test-export-accessors
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export-accessors
         #1=(defclass foo ()
              (slot0
               (slot1)
               (slot2 :reader x :reader y :accessor z :writer (setf p))
               (slot3 :reader foo))))
       '(progn
         (eval-when (:compile-toplevel :load-toplevel :execute) (export '(x y z p foo)))
         #1#)))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:export-accessors
         #2=(defclass bar () ())
         #3=(defclass baz () ((slot :accessor acc))))
       '(progn
         #2#
         (progn (eval-when (:compile-toplevel :load-toplevel :execute) (export '(acc)))
                #3#))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export-accessors
         #3=(format t "Hello, World!"))
       '#3#))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export-accessors)
       nil)))

(test test-export-class
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export-class
         #1=(defclass foo () ()))
       '(cl-annot-revisit:export-slots
         (cl-annot-revisit:export-accessors
           #1#))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export-class
         #2=(defclass foo ()
              (slot1
               (slot2 :reader slot2-read :writer slot2-write))))
       '(cl-annot-revisit:export-slots
         (cl-annot-revisit:export-accessors
           #2#))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export-class (list 1 2 3))
       '(list 1 2 3)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export-class)
       'nil)))
