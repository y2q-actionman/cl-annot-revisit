(in-package #:cl-annot-revisit-test)

(test test-@export-single
  (is (equal
       (macroexpand
        '(@export
          #1=(defconstant hoge 100)))
       '(progn
         (@eval-always (export 'hoge))
         #1#)))
  (is (equal
       (macroexpand
        '(@export
          #2=(defun (setf func) (val) val)))
       '(progn
         (@eval-always (export 'func))
         #2#)))
  (is (equal
       (macroexpand
        '(@export
          #3=(defstruct struct1
               slot1)))
       '(progn
         (@eval-always (export 'struct1))
         #3#)))
  (is (equal
       (macroexpand
        '(@export
          #4=(defstruct (struct2 :copier cpstruct2)
               slot1 slot2)))
       '(progn
         (@eval-always (export 'struct2))
         #4#)))
  t)

(test test-@export-other
  (is (equal
       (macroexpand
        '(@export
          #1=(format t "Hello, World!")))
       '#1#))
  t)

(test test-@export-many
  (is (equal
       (macroexpand
        '(@export
          #1=(defconstant hoge 100)
          #2=(defconstant fuga 200)))
       '(progn
         (@export #1#)
         (@export #2#))))
  (is (equal
       (macroexpand
        '(@export
          #3=(defconstant hoge 100)
          #4=(defun fuga () )))
       '(progn
         (@export #3#)
         (@export #4#))))
  t)
