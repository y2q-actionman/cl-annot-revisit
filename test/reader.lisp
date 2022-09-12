(in-package :cl-annot-revisit-test)

(defmacro within-at-syntax-readtable (&body body)
  `(let ((*readtable* (find-readtable 'cl-annot-revisit:at-syntax-readtable)))
     ,@body))

(in-readtable cl-annot-revisit:at-syntax-readtable)

(test test-at-syntax-symbol-empty
  (within-at-syntax-readtable
   (signals stream-error
     (read-from-string "@"))
   (signals stream-error
     (read-from-string "@)"))
   (signals stream-error
     (read-from-string "@ "))))

(test test-at-syntax-symbol-eval-when
  (is (equal '@cl-annot-revisit:eval-when-compile (1 2 3)
             '(cl-annot-revisit:eval-when-compile (1 2 3))))
  (is (equal '@cl-annot-revisit:eval-when-load (1 2 3)
             '(cl-annot-revisit:eval-when-load (1 2 3))))
  (is (equal '@cl-annot-revisit:eval-when-execute (+ 1 2 3)
             '(cl-annot-revisit:eval-when-execute (+ 1 2 3))))
  (is (equal-after-macroexpand
       '@cl-annot-revisit:eval-always
       ((lambda (x y z) (* x y z)) 100 200 300)
       '(eval-when (:compile-toplevel :load-toplevel :execute)
         ((lambda (x y z) (* x y z)) 100 200 300))))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@cl-annot-revisit:eval-always"))))

(test test-at-syntax-symbol-declaration
  (is (equal-after-macroexpand
       '@cl-annot-revisit:ignore x
       (defun foo (x) 9999)
       '(defun foo (x)
         (declare (ignore x))
         9999)))
  (is (equal-after-macroexpand
       '@cl-annot-revisit:ignorable (x)
       (defun foo (x) 9999)
       '(defun foo (x)
         (declare (ignorable x))
         9999)))
  (is (equal-after-macroexpand
       '@cl-annot-revisit:dynamic-extent (x)
       (defun foo (x) 9999)
       '(defun foo (x)
         (declare (dynamic-extent x))
         9999)))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@cl-annot-revisit:dynamic-extent"))
    (signals stream-error
      (read-from-string "@cl-annot-revisit:dynamic-extent (x)"))))
