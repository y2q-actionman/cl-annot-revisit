(in-package :cl-annot-revisit-test)

(test test-@metaclass
  (is (equal (macroexpand-1 '(cl-annot-revisit:metaclass bar
                              (defclass foo ()
                                ())))
             '(defclass foo ()
               ()
               (:metaclass bar))))
  (signals at-macro-error
    (macroexpand-1 '(cl-annot-revisit:metaclass bar
                     (defclass foo ()
                       ()
                       (:metaclass baz))))))

(test test-@export-slots
  (is (equal (macroexpand-1 '(cl-annot-revisit:export-slots
                              #1=(define-condition hoge () #2=(slot1 slot2))))
             '(progn
               (eval-when #.+at-macro-eval-always+ (export '#2#))
               #1#))))

(test test-@export-accessors
  (is (equal (macroexpand-1 '(cl-annot-revisit:export-accessors
                              #3=(defclass foo ()
                                   (slot0
                                    (slot1)
                                    (slot2 :reader x :reader y :accessor z :writer (setf p))))))
             '(progn
               (eval-when #.+at-macro-eval-always+ (export '(x y z p)))
               #3#))))

(test test-@export-class
  (is (equal (macroexpand-1 '(cl-annot-revisit:export-class
                              #3=(defclass foo () ())))
             '(cl-annot-revisit:export-slots (cl-annot-revisit:export-accessors (cl-annot-revisit:export #3#))))))

(test test-@export-accessors-defstruct
  ;; XXX: I must let `*package*' because 1am does not preserve current package
  ;; when it runs `macroexpand-1' which will call `intern' in defstruct parser.
  (is (equal (let ((*package* (find-package :cl-annot-revisit-test)))
               (macroexpand-1 '(cl-annot-revisit:export-accessors
                                #1=(defstruct foo slot1 slot2))))
             '(progn (eval-when #.+at-macro-eval-always+ (export '(foo-slot1 foo-slot2)))
               #1#))))
