(in-package #:cl-annot-revisit-test)

(test test-@export-single
  (is (equal
       (macroexpand
        '(cl-annot-revisit:export
          #1=(defconstant hoge 100)))
       '(progn
         (eval-when #.+at-macro-eval-always+ (export '(hoge)))
         #1#)))
  (is (equal
       (macroexpand
        '(cl-annot-revisit:export
          #2=(defun (setf func) (val) val)))
       '(progn
         (eval-when #.+at-macro-eval-always+ (export '(func)))
         #2#)))
  (is (equal
       (macroexpand
        '(cl-annot-revisit:export
          #3=(defstruct struct1
               slot1)))
       '(progn
         (eval-when #.+at-macro-eval-always+ (export '(struct1)))
         #3#)))
  (is (equal
       (macroexpand
        '(cl-annot-revisit:export
          #4=(defstruct (struct2 (:copier cpstruct2))
               slot1 slot2)))
       '(progn
         (eval-when #.+at-macro-eval-always+ (export '(struct2)))
         #4#)))
  t)

(test test-@export-other
  (is (equal
       (macroexpand
        '(cl-annot-revisit:export
          #1=(format t "Hello, World!")))
       '#1#))
  t)

(test test-@export-many
  (is (equal
       (macroexpand
        '(cl-annot-revisit:export
          #1=(defconstant hoge 100)
          #2=(defconstant fuga 200)))
       '(progn
         (cl-annot-revisit:export #1#)
         (cl-annot-revisit:export #2#))))
  (is (equal
       (macroexpand
        '(cl-annot-revisit:export
          #3=(defconstant hoge 100)
          #4=(defun fuga () )))
       '(progn
         (cl-annot-revisit:export #3#)
         (cl-annot-revisit:export #4#))))
  t)
