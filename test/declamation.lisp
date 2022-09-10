(in-package #:cl-annot-revisit-test)

;;; `special'

(test test-decl-special-inline
  (is (equal
       '(let ((x 100)) #.(cl-annot-revisit:special *x*) 99)
       '(let ((x 100)) (declare (special *x*)) 99)))
  (is (equal
       '(let ((x 1)) #.(cl-annot-revisit:special (*x* *x* *x*)) 0)
       '(let ((x 1)) (declare (special *x* *x* *x*)) 0)))
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

(test test-decl-special-one-body
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:special (x y z)
         #1=(defvar *hoge* 12345))
       '(locally (declare (special x y z))
         #1#)))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:special ()
         #2=(defvar *hoge* 12345))
       '(locally (declare (special))
         #2#)))
  (is (equal-after-macroexpand-all      ; ambiguous case.
       '(cl-annot-revisit:special
         #3=(defvar *hoge* 12345))
       '(progn
         (declaim (special *hoge*))
         #3#)))
  (is (or (equal-after-macroexpand-all
           '#4=(cl-annot-revisit:special
                   (defun foo (x) 12345))
           '(defun foo (x)
             (declare (special))        ; may or may not exist.
             12345))
          (equal-after-macroexpand-all
           '#4#
           '(defun foo (x)
             12345)))))

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
       '(let ((x 100)) #.(cl-annot-revisit:type integer xxxxx) 99)
       '(let ((x 100)) (declare (type integer xxxxx)) 99)))
  (is (equal
       '(let ((x 1)) #.(cl-annot-revisit:type (integer -1 +1) (xxxxx xxxxx xxxxx)) 0)
       '(let ((x 1)) (declare (type (integer -1 +1) xxxxx xxxxx xxxxx)) 0)))
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

(test test-decl-inline-no-body          ; will makes a `declaim' form.
  (is (starts-with-subseq
       '(progn (declaim (inline hoge fuga piyo)))
       (macroexpand
        '(cl-annot-revisit:inline (hoge fuga piyo)))
       :test 'equal))
  (is (starts-with-subseq
       '(progn (declaim (inline)))
       (macroexpand
        '(cl-annot-revisit:inline ()))
       :test 'equal))
  (is (starts-with-subseq
       '(progn (declaim (inline)))
       (macroexpand
        '(cl-annot-revisit:inline))
       :test 'equal)))

(test test-decl-inline-with-one-var    ; will add a declaration.
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline hoge
         (+ 1 2 3))
       '(locally
         (declare (inline hoge))
         (+ 1 2 3))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline hoge
         (defun hoge (x)
           9999))
       '(defun hoge (x)
         (declare (inline hoge))
         9999)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline hoge
         #1=(defvar *hoge* 12345))
       '(locally
         (declare (inline hoge))
         #1#))))

(test test-function-definitions-operator-p
  ;; applicable
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline hoge
         (defun hoge (x) 100))
       '(defun hoge (x)
         (declare (inline hoge))
         100)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline hoge
         (defmethod hoge (x) 200))
       '(defmethod hoge (x)
         (declare (inline hoge))
         200)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline hoge
         (define-compiler-macro hoge (x)
           9999))
       '(define-compiler-macro hoge (x)
         (declare (inline hoge))
         9999)))
  ;; not applicable
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline hoge
         (defgeneric hoge (x))) ; `defgeneric' accepts only `optimize'.
       '(locally (declare (inline hoge))
         (defgeneric hoge (x)))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline hoge
         (define-method-combination hoge))
       '(locally (declare (inline hoge))
         (define-method-combination hoge))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline hoge
         (defsetf hoge (x y) (store)
           9999))
       '(defsetf hoge (x y) (store)
         (declare (inline hoge))
         9999))))

(test test-decl-inline-with-vars       ; will add a declaration.
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline (hoge fuga piyo)
         (+ 1 2 3))
       '(locally
         (declare (inline hoge fuga piyo))
         (+ 1 2 3))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline (hoge)
         (defun hoge (x)
           9999))
       '(defun hoge (x)
         (declare (inline hoge))
         9999)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline (hoge)
         #1=(defvar *hoge* 12345))
       '(locally
         (declare (inline hoge))
         #1#))))

(test test-decl-inline-with-nil        ; will add an empty declaration.
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline ()
         (+ 1 2 3))
       '(locally
         (declare (inline))
         (+ 1 2 3))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline ()
         (defun hoge (x)
           9999))
       '(defun hoge (x)
         (declare (inline))
         9999)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline ()
         #1=(defvar *hoge* 12345))
       '(locally
         (declare (inline))
         #1#))))

(test test-decl-inline-without-vars ; will add a `declaim' form if some variable is defined.
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline 
         #1=(+ 1 2 3))
       '#1#))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline
         #2=(defun hoge (x)
              9999))
       '(progn
         (declaim (inline hoge))
         #2#)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline
         #3=(defvar *hoge* 12345))
       '#3#)))

(test test-decl-inline-multiforms
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:inline hoge
         (defun foo (x))
         (format t "Hello, World!")
         (defvar *foo* 9999)
         (defmethod bar ()
           "docstring"
           ""))
       '#1=(progn
             (defun foo (x)
               (declare (inline hoge)))
             (locally
                 (declare (inline hoge))
               (format t "Hello, World!"))
             (locally
                 (declare (inline hoge))
               (defvar *foo* 9999))
             (defmethod bar ()
               (declare (inline hoge))
               "docstring"
               "")
             )))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:inline (hoge)
         (defun foo (x))
         (format t "Hello, World!")
         (defvar *foo* 9999)
         (defmethod bar ()
           "docstring"
           ""))
       '#1#))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:inline
         (defun foo (x))
         (format t "Hello, World!")
         (defvar *foo* 9999)
         (defmethod bar ()
           "docstring"
           ""))
       '(progn
         (progn (declaim (inline foo))
                (defun foo (x)))
         (format t "Hello, World!")
         (defvar *foo* 9999)
         (progn (declaim (inline bar))
                (defmethod bar ()
                  "docstring"
                  ""))))))

(test test-decl-inline-ambiguous-multiforms
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:inline (list a b c)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (defun foo (x) (declare (inline list a b c)))
         (locally (declare (inline list a b c))
            "Hello, World!")
         (locally (declare (inline list a b c))
           (defvar *foo* 9999)))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:inline (list 1 2 3)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (list 1 2 3)
         (progn (declaim (inline foo))
                (defun foo (x)))
         "Hello, World!"
         (defvar *foo* 9999))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:inline (define-method-combination hoge)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (define-method-combination hoge)
         (progn (declaim (inline foo))
                (defun foo (x)))
         "Hello, World!"
         (defvar *foo* 9999))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:inline (progn lambda-list-keywords)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (progn lambda-list-keywords)
         (progn (declaim (inline foo))
                (defun foo (x)))
         "Hello, World!"
         (defvar *foo* 9999)))))

;;; `notinline'

(test test-decl-notinline-ambiguous-multiforms
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:notinline (list a b c)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (defun foo (x) (declare (notinline list a b c)))
         (locally (declare (notinline list a b c))
            "Hello, World!")
         (locally (declare (notinline list a b c))
           (defvar *foo* 9999)))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:notinline (list 1 2 3)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (list 1 2 3)
         (progn (declaim (notinline foo))
                (defun foo (x)))
         "Hello, World!"
         (defvar *foo* 9999))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:notinline (define-method-combination hoge)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (define-method-combination hoge)
         (progn (declaim (notinline foo))
                (defun foo (x)))
         "Hello, World!"
         (defvar *foo* 9999))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:notinline (progn lambda-list-keywords)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (progn lambda-list-keywords)
         (progn (declaim (notinline foo))
                (defun foo (x)))
         "Hello, World!"
         (defvar *foo* 9999)))))

;;; `ftype'

(test test-decl-ftype-ambiguous-multiforms
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ftype #1=(function (t) t) (list a b c)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (defun foo (x)
           (declare (ftype #1# list a b c)))
         (locally (declare (ftype #1# list a b c))
           "Hello, World!")
         (locally (declare (ftype #1# list a b c))
           (defvar *foo* 9999)))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ftype #2=(function (t) null) (list 1 2 3)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (list 1 2 3)
         (progn (declaim (ftype #2# foo))
                (defun foo (x)))
         "Hello, World!"
         (defvar *foo* 9999))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ftype #3=(function (t) null) (define-method-combination hoge)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (define-method-combination hoge)
         (progn (declaim (ftype #3# foo))
                (defun foo (x)))
         "Hello, World!"
         (defvar *foo* 9999))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ftype (function) (setf baz)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (defun foo (x)
           (declare #4=(ftype (function) (setf baz))))
         (locally (declare #4#)
           "Hello, World!")
         (locally (declare #4#)
           (defvar *foo* 9999))))))

;;; `optimize'

(test test-decl-optimize-inline
  (is (equal
       '(let ((x 100)) #.(cl-annot-revisit:optimize (speed 3)) 99)
       '(let ((x 100)) (declare (optimize (speed 3))) 99)))
  (is (equal
       '(let ((x 1)) #.(cl-annot-revisit:optimize speed) 0)
       '(let ((x 1)) (declare (optimize speed)) 0)))
  (is (equal
       '(let ((x 1)) #.(cl-annot-revisit:optimize) 0)
       '(let ((x 1)) (declare (optimize)) 0))))

(test test-decl-optimize-no-body         ; will makes a `declaim' form.
  #+FIXME
  (is (starts-with-subseq
       '(progn (declaim (optimize speed debug)))
       (macroexpand
        '(cl-annot-revisit:optimize (speed debug)))
       :test 'equal))
  (is (starts-with-subseq
       '(progn (declaim (optimize)))
       (macroexpand
        '(cl-annot-revisit:optimize ()))
       :test 'equal))
  (is (starts-with-subseq
       '(progn (declaim (optimize)))
       (macroexpand
        '(cl-annot-revisit:optimize))
       :test 'equal)))

(test test-decl-optimize-function-definitions-operator-p
  (is (equal-after-macroexpand
       '(cl-annot-revisit:optimize (speed 0) 
         (defun hoge (x) 100))
       '(defun hoge (x)
         (declare (optimize (speed 0)))
         100)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:optimize ((speed 3) (safety 0))
         (defmethod hoge (x) 200))
       '(defmethod hoge (x)
         (declare (optimize (speed 3) (safety 0)))
         200)))
  #+FIXME
  (is (equal-after-macroexpand
       '(cl-annot-revisit:optimize (speed debug)
         (define-compiler-macro hoge (x)
          9999))
       '(define-compiler-macro hoge (x)
         (declare (optimize speed debug))
         9999)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:optimize ((speed 1))
         (defsetf hoge (x y) (store)
           9999))
       '(defsetf hoge (x y) (store)
         (declare (optimize (speed 1)))
         9999)))
  ;; applicable only by `optimize'
  (is (equal-after-macroexpand
       '(cl-annot-revisit:optimize compilation-speed
         (defgeneric hoge (x)))
       '(defgeneric hoge (x)
         (declare (optimize compilation-speed)))))
  ;; not applicable
  (is (equal-after-macroexpand
       '(cl-annot-revisit:optimize debug
         (define-method-combination hoge))
       '(locally (declare (optimize debug))
         (define-method-combination hoge)))))

(test test-decl-optimize-ambiguous-multiforms
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:optimize (list a b c)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (defun foo (x) (declare (optimize (list a b c))))
         (locally (declare (optimize (list a b c)))
           "Hello, World!")
         (locally (declare (optimize (list a b c)))
           (defvar *foo* 9999)))))
  ;; These tests are ambiguous.
  ;; They make unsyntactic declarations, but
  ;; `cl-annot-revisit:optimize' accept them because I think it should
  ;; accept implementation-dependent optimization switches.
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:optimize (list 1 2 3)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (defun foo (x) (declare (optimize (list 1 2 3))))
         (locally (declare (optimize (list 1 2 3)))
           "Hello, World!")
         (locally (declare (optimize (list 1 2 3)))
           (defvar *foo* 9999)))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:optimize (define-method-combination hoge)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (defun foo (x) (declare (optimize (define-method-combination hoge))))
         (locally (declare (optimize (define-method-combination hoge)))
           "Hello, World!")
         (locally (declare (optimize (define-method-combination hoge)))
           (defvar *foo* 9999)))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:optimize (progn lambda-list-keywords)
         (defun foo (x))
         "Hello, World!"
         (defvar *foo* 9999))
       '(progn
         (defun foo (x) (declare (optimize (progn lambda-list-keywords))))
         (locally (declare (optimize (progn lambda-list-keywords)))
           "Hello, World!")
         (locally (declare (optimize (progn lambda-list-keywords)))
           (defvar *foo* 9999))))))

;;; combination

(test test-decl-type-and-ftype
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:type integer
         (cl-annot-revisit:ftype function
             "Hello, World!"
           (defun foo (x))
           (defvar *foo* 9999)))
       '(progn
         "Hello, World!"
         (progn (declaim (ftype function foo))
                (defun foo (x)))
         (progn (declaim (type integer *foo*))
                (defvar *foo* 9999)))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:type integer (x)
         (cl-annot-revisit:ftype function (func)
             "Hello, World!"
           (defun foo (x) -1)
           (defvar *foo* 9999)))
       '(progn
         (locally #1=(declare (type integer x))
                  #2=(declare (ftype function func))
           "Hello, World!")
         (defun foo (x)
           #1# #2#
           -1)
         (locally #1# #2#
           (defvar *foo* 9999))))))

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
