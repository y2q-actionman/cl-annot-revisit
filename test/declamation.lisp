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

(test test-decl-special-multiforms
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:special x
         (defun foo (x))
         (format t "Hello, World!")
         (defvar *foo* 9999)
         (defmethod bar ()
           "docstring"
           ""))
       '#1=(progn
             (defun foo (x)
               (declare (special x)))
             (locally
                 (declare (special x))
               (format t "Hello, World!"))
             (locally
                 (declare (special x))
               (defvar *foo* 9999))
             (defmethod bar ()
               (declare (special x))
               "docstring"
               "")
             )))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:special (x)
         (defun foo (x))
         (format t "Hello, World!")
         (defvar *foo* 9999)
         (defmethod bar ()
           "docstring"
           ""))
       '#1#))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:special
         (defun foo (x))
         (format t "Hello, World!")
         (defvar *foo* 9999)
         (defmethod bar ()
           "docstring"
           ""))
       '(progn
         (defun foo (x))
         (format t "Hello, World!")
         (progn (declaim (special *foo*))
                (defvar *foo* 9999))
         (defmethod bar ()
           "docstring"
           "")))))

(test test-decl-special-ambiguous-multiforms
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:special (list a b c)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (defun foo (x) (declare (special list a b c)))
         (locally (declare (special list a b c))
            "Hello, World!")
         (locally (declare (special list a b c))
           (defvar *foo* 9999)))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:special (list 1 2 3)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (list 1 2 3)
         (defun foo (x))
         "Hello, World!"
         (progn (declaim (special *foo*))
                (defvar *foo* 9999)))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:special (define-method-combination hoge)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (define-method-combination hoge)
         (defun foo (x))
         "Hello, World!"
         (progn (declaim (special *foo*))
                (defvar *foo* 9999)))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:special (progn lambda-list-keywords)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (progn lambda-list-keywords)
         (defun foo (x))
         "Hello, World!"
         (progn (declaim (special *foo*))
                (defvar *foo* 9999))))))

;;; `type'

(test test-decl-type-inline
  (is (equal
       '(let ((x 100)) #.(cl-annot-revisit:type integer x) 99)
       '(let ((x 100)) (declare (type integer x)) 99)))
  (is (equal
       '(let ((x 1)) #.(cl-annot-revisit:type (integer -1 +1) (x x x)) 0)
       '(let ((x 1)) (declare (type (integer -1 +1) x x x)) 0)))
  (is (equal
       '(let ((x 1)) #.(cl-annot-revisit:type integer ()) 0)
       '(let ((x 1)) (declare (type integer)) 0))))

(test test-decl-type-no-body         ; will makes a `declaim' form.
  (is (starts-with-subseq
       '(progn (declaim (type fixnum x y z)))
       (macroexpand
        '(cl-annot-revisit:type fixnum (x y z)))
       :test 'equal)))

(test test-decl-type-with-one-var       ; will makes a `declaim' form.
  (is (equal-after-macroexpand
       '(cl-annot-revisit:type integer x
         (+ 1 2 3))
       '(locally
         (declare (type integer x))
         (+ 1 2 3))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:type (integer -1) x
         (defun hoge (x)
           9999))
       '(defun hoge (x)
         (declare (type (integer -1) x))
         9999))))

(test test-decl-type-multiforms
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:type (integer -1) x
         (defun foo (x))
         (format t "Hello, World!")
         (defvar *foo* 9999))
       '(progn
         (defun foo (x)
           (declare (type (integer -1) x)))
         (locally
             (declare (type (integer -1) x))
           (format t "Hello, World!"))
         (locally
             (declare (type (integer -1) x))
           (defvar *foo* 9999))
         )))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:type (integer 9999)
         (defun foo (x))
         (format t "Hello, World!")
         (defvar *foo* 9999))
       '(progn
         (defun foo (x))
         (format t "Hello, World!")
         (progn (declaim (type (integer 9999) *foo*))
                (defvar *foo* 9999))))))

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