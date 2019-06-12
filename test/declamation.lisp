(in-package #:cl-annot-revisit-test)

(test test-decl-declararion
  (is (equal
       (macroexpand-1 '(@declaration (hoge fuga)))
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

(test test-decl-inline
  (is (equal
       (macroexpand '(@inline #1=(defun hoge ())))
       '(progn (declaim (inline hoge)) #1#))))