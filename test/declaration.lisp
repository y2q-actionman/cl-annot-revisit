(in-package #:cl-annot-revisit-test)

(test test-declaration-ignore
  (is (equal
       '(let ((x 100)) #.(@ignore x) 99)
       '(let ((x 100)) (declare (ignore x)) 99)))
  (is (equal
       '(let ((x 1)) #.(@ignore (x x x)) 0)
       '(let ((x 1)) (declare (ignore x x x)) 0))))
