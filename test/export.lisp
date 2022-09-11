(in-package #:cl-annot-revisit-test)

(test test-@export-single
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export
         #1=(defconstant hoge 100))
       '(progn
         (eval-when (:compile-toplevel :load-toplevel :execute) (export '(hoge)))
         #1#)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export
         #2=(defun (setf func) (val) val))
       '(progn
         (eval-when (:compile-toplevel :load-toplevel :execute) (export '(func)))
         #2#)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export
         #3=(defstruct struct1
              slot1))
       '(progn
         (eval-when (:compile-toplevel :load-toplevel :execute) (export '(struct1)))
         #3#)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export
         #4=(defstruct (struct2 (:copier cpstruct2))
              slot1 slot2))
       '(progn
         (eval-when (:compile-toplevel :load-toplevel :execute) (export '(struct2)))
         #4#))))

(test test-@export-other
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export
         #1=(format t "Hello, World!"))
       '#1#)))

(test test-@export-many
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export
         #1=(defconstant hoge 100)
         #2=(defconstant fuga 200))
       '(progn
         (cl-annot-revisit:export #1#)
         (cl-annot-revisit:export #2#))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export
         #3=(defconstant hoge 100)
         #4=(defun fuga () ))
       '(progn
         (cl-annot-revisit:export #3#)
         (cl-annot-revisit:export #4#)))))
