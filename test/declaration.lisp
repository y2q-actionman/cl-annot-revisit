(in-package #:cl-annot-revisit-test)

(test test-decl-declararion
  (is (equal
       (macroexpand-1 '(@declaration (hoge fuga)))
       '(declaim (declaration hoge fuga))))
  ;; A test for the standard Common Lisp.
  ;; This is malformed, because `declaration' is only for proclamation.
  (signals (or error warning)
    (compile nil
             '(lambda ()
               (declare (declaration hoge))
               t)))
  #+ ()
  (signals (or error warning)
    (compile nil
             '(lambda ()
               ;; The result of `declaim' is implementation-dependent. This code add such a value.
               #.(@declaration hoge) 
               t))))

(test test-decl-ignore
  (is (equal
       '(let ((x 100)) #.(@ignore x) 99)
       '(let ((x 100)) (declare (ignore x)) 99)))
  (is (equal
       '(let ((x 1)) #.(@ignore (x x x)) 0)
       '(let ((x 1)) (declare (ignore x x x)) 0))))

(test test-decl-inline
  (is (equal
       (macroexpand '(@inline #1=(defun hoge ())))
       '(progn (declaim (inline hoge)) #1#))))
