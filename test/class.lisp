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
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export-slots
         (define-condition hoge () #1=(slot1 slot2)))
       '#1#))
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
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export-accessors
         (defclass foo ()
           (slot0
            (slot1)
            (slot2 :reader x :reader y :accessor z :writer (setf p))
            (slot3 :reader foo))))
       '(x y z p foo)))
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
           (cl-annot-revisit:export
             #1#)))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export-class
         #2=(defclass foo ()
              (slot1
               (slot2 :reader slot2-read :writer slot2-write))))
       '(cl-annot-revisit:export-slots
         (cl-annot-revisit:export-accessors
           (cl-annot-revisit:export
             #2#)))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export-class (list 1 2 3))
       '(list 1 2 3)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export-class)
       'nil)))
