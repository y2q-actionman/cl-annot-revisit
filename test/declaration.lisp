(in-package #:cl-annot-revisit-test)

(test test-decl-ignore
  (is (equal
       '(let ((x 100)) #.(cl-annot-revisit:ignore x) 99)
       '(let ((x 100)) (declare (ignore x)) 99)))
  (is (equal
       '(let ((x 1)) #.(cl-annot-revisit:ignore (x x x)) 0)
       '(let ((x 1)) (declare (ignore x x x)) 0))))
