(in-package :cl-annot-revisit-test)

(named-readtables:in-readtable cl-annot-revisit:at-syntax-readtable)

(test test-readme-example-1
  (is (equal
       '@cl-annot-revisit:export
       @cl-annot-revisit:optimize ((speed 3) (safety 0))
       (defun foo ()
         "Hello, World!")
       '#1=(cl-annot-revisit:export
             (cl-annot-revisit:optimize ((speed 3) (safety 0))
               (defun foo ()
                 "Hello, World!")))))
  (is (equal-after-macroexpand-all
       '#1#
       '(progn (eval-when (:compile-toplevel :load-toplevel :execute)
                 (export '(foo)))
         (defun foo ()
           (declare (optimize (speed 3) (safety 0)))
           "Hello, World!")))))

@cl-annot-revisit:export
@cl-annot-revisit:optimize ((speed 3) (safety 0))
(defun foo ()
  "Hello, World!")

(test test-readme-example-1-func
  (is (equal (foo) "Hello, World!")))
