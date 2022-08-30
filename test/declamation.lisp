(in-package #:cl-annot-revisit-test)

(test test-decl-special-inline
  (is (equal
       '(let ((x 100)) #.(cl-annot-revisit:special x) 99)
       '(let ((x 100)) (declare (special x)) 99)))
  (is (equal
       '(let ((x 1)) #.(cl-annot-revisit:special (x x x)) 0)
       '(let ((x 1)) (declare (special x x x)) 0)))
  (is (equal
       '(let ((x 1)) #.(cl-annot-revisit:special ()) 0)
       '(let ((x 1)) (declare (special)) 0))))

(test test-decl-inline
  (is (equal
       (macroexpand '(cl-annot-revisit:inline #1=(defun hoge ())))
       '(progn (declaim (inline hoge)) #1#))))

(test test-decl-declararion
  (is (equal
       (macroexpand-1 '(cl-annot-revisit:declaration (hoge fuga)))
       '(declaim (declaration hoge fuga))))
  ;; A test for the standard Common Lisp.
  ;; This is malformed, because `declaration' is only for proclamation.
  (is (nth-value
       2                                ; sees FAILURE-P of `compile'.
       (let ((*error-output* (make-broadcast-stream)))
         (compile nil
                  '(lambda ()
                    (declare (declaration hoge)))))))
  #+ ()
  (signals (or error warning)
    (compile nil
             '(lambda ()
               ;; The result of `declaim' is implementation-dependent. This code add such a value.
               #.(@declaration hoge)
               t))))

;;; TODO
;;; - Use special and type to test variable-definition-operator-p
;;; - Useftype, inline, notinline to test function-definition-operator-p