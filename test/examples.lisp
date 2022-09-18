(in-package :cl-annot-revisit-test)

(named-readtables:in-readtable cl-annot-revisit:at-syntax-readtable)

(test test-readme-example-1
  (is (equal
       '@cl-annot-revisit:export
       @(cl-annot-revisit:optimize ((speed 3) (safety 0)))
       (cl-annot-revisit:inline
           (defun foo ()
             "Hello, World!")
         (defun bar (x)
           (1+ x)))
       '#1=(cl-annot-revisit:export
             (cl-annot-revisit:optimize ((speed 3) (safety 0))
               (cl-annot-revisit:inline
                   (defun foo ()
                     "Hello, World!")
                 (defun bar (x)
                   (1+ x)))))))
  (let ((expansion (macroexpand-all '#1#)))
    (is (find-in-nested-progn
         (macroexpand-all
          '(eval-when (:compile-toplevel :load-toplevel :execute)
            (export '(foo))))
         expansion))
    (is (find-in-nested-progn
         (macroexpand-all
          '(eval-when (:compile-toplevel :load-toplevel :execute)
            (export '(bar))))
         expansion))
    (is (find-in-nested-progn
         (macroexpand-all
          '(defun foo ()
            (declare (optimize (speed 3) (safety 0)))
            "Hello, World!"))
         expansion))
    (is (find-in-nested-progn
         (macroexpand-all
          '(defun bar (x)
            (declare (optimize (speed 3) (safety 0)))
            (1+ x)))
         expansion))
    (is (find-in-nested-progn
         (macroexpand-all
          '(declaim
            (optimize (speed 3) (safety 0))
            (inline foo)))
         expansion))
    (is (find-in-nested-progn
         (macroexpand-all
          '(declaim
            (optimize (speed 3) (safety 0))
            (inline bar)))
         expansion))))

@cl-annot-revisit:export
@(cl-annot-revisit:optimize ((speed 3) (safety 0)))
(cl-annot-revisit:inline
    (defun foo ()
      "Hello, World!")
  (defun bar (x)
    (1+ x)))

(test test-readme-example-1-func
  (is (equal (foo) "Hello, World!"))
  (is (equal (bar 1) 2)))

(test test-readme-example-1-symbol
  (is (eq (nth-value 1 (find-symbol (string 'foo) :cl-annot-revisit-test))
          :external))
  (is (eq (nth-value 1 (find-symbol (string 'bar) :cl-annot-revisit-test))
          :external)))
