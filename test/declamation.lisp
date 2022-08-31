(in-package #:cl-annot-revisit-test)

;;; `special'

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

(test test-decl-special-no-body         ; will makes a `declaim' form.
  (is (starts-with-subseq
       '(progn (declaim (special x y z)))
       (macroexpand
        '(cl-annot-revisit:special (x y z)))
       :test 'equal))
  (is (starts-with-subseq
       '(progn (declaim (special)))
       (macroexpand
        '(cl-annot-revisit:special ()))
       :test 'equal))
  (is (starts-with-subseq
       '(progn (declaim (special)))
       (macroexpand
        '(cl-annot-revisit:special))
       :test 'equal)))

(test test-decl-special-with-one-var    ; will add a declaration.
  (is (equal-after-macroexpand
       '(cl-annot-revisit:special x
         (+ 1 2 3))
       '(locally
         (declare (special x))
         (+ 1 2 3))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:special x
         (defun hoge (x)
           9999))
       '(defun hoge (x)
         (declare (special x))
         9999)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:special x
         #1=(defvar *hoge* 12345))
       '(locally
         (declare (special x))
         #1#))))

(test test-decl-special-with-vars       ; will add a declaration.
  (is (equal-after-macroexpand
       '(cl-annot-revisit:special (x y z)
         (+ 1 2 3))
       '(locally
         (declare (special x y z))
         (+ 1 2 3))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:special (x)
         (defun hoge (x)
           9999))
       '(defun hoge (x)
         (declare (special x))
         9999)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:special (x)
         #1=(defvar *hoge* 12345))
       '(locally
         (declare (special x))
         #1#))))

(test test-decl-special-with-nil        ; will add an empty declaration.
  (is (equal-after-macroexpand
       '(cl-annot-revisit:special ()
         (+ 1 2 3))
       '(locally
         (declare (special))
         (+ 1 2 3))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:special ()
         (defun hoge (x)
           9999))
       '(defun hoge (x)
         (declare (special))
         9999)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:special ()
         #1=(defvar *hoge* 12345))
       '(locally
         (declare (special))
         #1#))))

(test test-decl-special-without-vars    ; will add a `declaim' form if some variable is defined.
  (is (equal-after-macroexpand
       '(cl-annot-revisit:special 
         #1=(+ 1 2 3))
       '#1#))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:special
         #2=(defun hoge (x)
              9999))
       '#2#))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:special
         #3=(defvar *hoge* 12345))
       '(progn
         (declaim (special *hoge*))
         #3#))))


;; (test test-decl-special-multiforms
;;   )

;; (test test-decl-special-ambiguous-multiforms
;;   )

;;; `inline'

(test test-decl-inline
  (is (equal
       (macroexpand '(cl-annot-revisit:inline #1=(defun hoge ())))
       '(progn (declaim (inline hoge)) #1#))))

;;; `declaration'

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