(in-package :cl-annot-revisit-test)

(defmacro within-at-syntax-readtable (&body body)
  `(let ((*readtable* (find-readtable 'cl-annot-revisit:at-syntax-readtable)))
     ,@body))

(in-readtable cl-annot-revisit:at-syntax-readtable)

(test test-at-syntax-error
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@"))
    (signals stream-error
      (read-from-string "@)"))
    (signals stream-error
      (read-from-string "@ "))
    (signals stream-error
      (read-from-string "@("))
    (signals stream-error
      (read-from-string "@(err"))))

;;; Test at-macro application.

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

(test test-at-syntax-list-eval-when
  (is (equal '@(cl-annot-revisit:eval-when-compile) (1 2 3)
             '(cl-annot-revisit:eval-when-compile (1 2 3))))
  (is (equal '@(cl-annot-revisit:eval-when-load) (1 2 3)
             '(cl-annot-revisit:eval-when-load (1 2 3))))
  (is (equal '@(cl-annot-revisit:eval-when-execute) (+ 1 2 3)
             '(cl-annot-revisit:eval-when-execute (+ 1 2 3))))
  (is (equal-after-macroexpand
       '@(cl-annot-revisit:eval-always)
       ((lambda (x y z) (* x y z)) 100 200 300)
       '(eval-when (:compile-toplevel :load-toplevel :execute)
         ((lambda (x y z) (* x y z)) 100 200 300))))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@(cl-annot-revisit:eval-always)"))))

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

(test test-at-syntax-list-declaration
  (is (equal-after-macroexpand
       '@(cl-annot-revisit:ignore x)
       (defun foo (x) 9999)
       '(defun foo (x)
         (declare (ignore x))
         9999)))
  (is (equal-after-macroexpand
       '@(cl-annot-revisit:ignorable (x))
       (defun foo (x) 9999)
       '(defun foo (x)
         (declare (ignorable x))
         9999)))
  (is (equal-after-macroexpand
       '@(cl-annot-revisit:dynamic-extent (x))
       (defun foo (x) 9999)
       '(defun foo (x)
         (declare (dynamic-extent x))
         9999)))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@(cl-annot-revisit:dynamic-extent)"))
    (signals stream-error
      (read-from-string "@(cl-annot-revisit:dynamic-extent (x))"))))

(test test-at-syntax-symbol-declamation
  (is (equal-after-macroexpand-all
       '@cl-annot-revisit:special
       (defvar *foo*)
       '(progn (declaim (special *foo*))
         (defvar *foo*))))
  (is (equal-after-macroexpand-all
       '@cl-annot-revisit:type integer
       (defvar *foo*)
       '(progn (declaim (type integer *foo*))
         (defvar *foo*))))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@cl-annot-revisit:type"))
    (signals stream-error
      (read-from-string "@cl-annot-revisit:type t")))
  (is (equal-after-macroexpand
       '@cl-annot-revisit:ftype (function)
       (defun foo (x) 9999)
       '(progn (declaim (ftype (function) foo))
         (defun foo (x) 9999))))
  (is (equal-after-macroexpand
       '@cl-annot-revisit:inline
       (defun bar (x))
       '(progn (declaim (inline bar))
         (defun bar (x)))))
  (is (equal-after-macroexpand
       '@cl-annot-revisit:notinline
       (defmethod baz (x))
       '(progn (declaim (notinline baz))
         (defmethod baz (x)))))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@cl-annot-revisit:notinline")))
  (is (equal-after-macroexpand
       '@cl-annot-revisit:optimize (speed)
       (defgeneric hoge (x))
       '(defgeneric hoge (x)
         (declare (optimize speed)))))
  (is (equal-after-macroexpand
       '@cl-annot-revisit:optimize (speed 3)
       (defgeneric hoge (x))
       '(defgeneric hoge (x)
         (declare (optimize (speed 3))))))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@cl-annot-revisit:optimize")))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@cl-annot-revisit:optimize (speed 3)"))))

(test test-at-syntax-list-declamation
  (is (equal-after-macroexpand-all
       '@(cl-annot-revisit:special)
       (defvar *foo*)
       '(progn (declaim (special *foo*))
         (defvar *foo*))))
  (is (equal-after-macroexpand-all
       '@(cl-annot-revisit:special (x)) ; @(list) syntax only
       (defun foo (x) (1+ x))
       '(defun foo (x)
         (declare (special x))
         (1+ x))))
  (is (equal-after-macroexpand-all
       '@(cl-annot-revisit:type integer)
       (defvar *foo*)
       '(progn (declaim (type integer *foo*))
         (defvar *foo*))))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@(cl-annot-revisit:type)"))
    (signals stream-error
      (read-from-string "@(cl-annot-revisit:type t)")))
  (is (equal-after-macroexpand
       '@(cl-annot-revisit:ftype (function))
       (defun foo (x) 9999)
       '(progn (declaim (ftype (function) foo))
         (defun foo (x) 9999))))
  (is (equal-after-macroexpand
       '@(cl-annot-revisit:inline)
       (defun bar (x))
       '(progn (declaim (inline bar))
         (defun bar (x)))))
  (is (equal-after-macroexpand
       '@(cl-annot-revisit:inline (hoge)) ; @(list) syntax only
       (defun bar (x))
       '(defun bar (x)
         (declare (inline hoge)))))
  (is (equal-after-macroexpand
       '@(cl-annot-revisit:notinline)
       (defmethod baz (x))
       '(progn (declaim (notinline baz))
         (defmethod baz (x)))))
  (is (equal-after-macroexpand
       '@(cl-annot-revisit:notinline fuga) ; @(list) syntax only
       (defmethod baz (x))
       '(defmethod baz (x)
         (declare (notinline fuga)))))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@(cl-annot-revisit:notinline)")))
  (is (equal-after-macroexpand
       '@(cl-annot-revisit:optimize (speed))
       (defgeneric hoge (x))
       '(defgeneric hoge (x)
         (declare (optimize speed)))))
  (is (equal-after-macroexpand
       '@(cl-annot-revisit:optimize (speed 3))
       (defgeneric hoge (x))
       '(defgeneric hoge (x)
         (declare (optimize (speed 3))))))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@(cl-annot-revisit:optimize)")))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@(cl-annot-revisit:optimize (speed 3))"))))

(test test-at-syntax-symbol-documentation
  (is (equal-after-macroexpand
       '@cl-annot-revisit:documentation "docstring"
       #1=(defun foo (x))
       '(let ((#:obj #1#))
         (setf (documentation #:obj 'function) "docstring")
         #:obj)))
  (is (equal-after-macroexpand
       '@cl-annot-revisit:doc "docstring"
       #2=#'(lambda bar (x))
       '(let ((#:obj #2#))
         (setf (documentation #:obj 'function) "docstring")
         #:obj)))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@cl-annot-revisit:documentation"))
    (signals stream-error
      (read-from-string "@cl-annot-revisit:doc"))))

(test test-at-syntax-list-documentation
  (is (equal-after-macroexpand
       '@(cl-annot-revisit:documentation "docstring")
       #1=(defun foo (x))
       '(let ((#:obj #1#))
         (setf (documentation #:obj 'function) "docstring")
         #:obj)))
  (is (equal-after-macroexpand
       '@(cl-annot-revisit:doc "docstring")
       #2=#'(lambda bar (x))
       '(let ((#:obj #2#))
         (setf (documentation #:obj 'function) "docstring")
         #:obj)))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@(cl-annot-revisit:documentation)"))
    (signals stream-error
      (read-from-string "@(cl-annot-revisit:doc)"))))

(test test-at-syntax-symbol-export
  (is (expanded-export-name-equalp
       '@cl-annot-revisit:export
       (defun foo (x) 9999)
       '(foo)))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@cl-annot-revisit:export"))))

(test test-at-syntax-list-export
  (is (expanded-export-name-equalp
       '@(cl-annot-revisit:export)
       (defun foo (x) 9999)
       '(foo)))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@(cl-annot-revisit:export)"))))

(test test-at-syntax-symbol-slots
  (is (equal
       '(defclass foo ()
         (slot0
          #.@cl-annot-revisit:optional initform1 slot1
          #.@cl-annot-revisit:optional initform2 (slot2 :initarg xxx)
          #.@cl-annot-revisit:optional initform3 (slot3 :initarg yyy :initform zzz)))
       '(defclass foo ()
         (slot0
          (slot1 :initform initform1 :initarg :slot1)
          (slot2 :initform initform2 :initarg xxx)
          (slot3 :initarg yyy :initform zzz))))))

(test test-at-syntax-list-slots
  (is (equal
       '(defclass foo ()
         (slot0
          #.@(cl-annot-revisit:optional initform1) slot1
          #.@(cl-annot-revisit:optional initform2) (slot2 :initarg xxx)
          #.@(cl-annot-revisit:optional initform3) (slot3 :initarg yyy :initform zzz)))
       '(defclass foo ()
         (slot0
          (slot1 :initform initform1 :initarg :slot1)
          (slot2 :initform initform2 :initarg xxx)
          (slot3 :initarg yyy :initform zzz))))))

;;; Test other syntaxes.

(test test-at-syntax-lambda-form
  (is (equal
       '@ (lambda (x y z) (+ x y z)) 1 20 300
       '((lambda (x y z) (+ x y z)) 1 20 300))))


(defmethod cl-annot-revisit:find-at-syntax-arity ((op (eql 'not-annot-op)) _)
  (declare (ignorable op _))
  nil)

(test test-at-syntax-not-annot
  (within-at-syntax-readtable
    (let ((*package* (find-package :cl-annot-revisit-test)))
      (let ((*at-macro-verbose* t))
        (signals at-macro-style-warning
          (read-from-string "@200"))
        (signals at-macro-style-warning
          (read-from-string "@not-annot-op"))
        (signals at-macro-style-warning
          (read-from-string "@()")))
      (let ((*at-macro-verbose* nil))
        (with-input-from-string (stream "@200 100")
          (eql (read stream) 200)
          (eql (read stream) 100))
        (with-input-from-string (stream "@not-annot-op @not-annot-op")
          (eql (read stream) 'not-annot-op)
          (eql (read stream) 'not-annot-op))
        (with-input-from-string (stream "@() nil")
          (eql (read stream) nil)
          (eql (read stream) nil)))
      ;; @(list) syntax does not see the symbol's arity.
      (is (equal
           '@ (not-annot 100 200) 300
           '(not-annot 100 200 300))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod cl-annot-revisit:find-at-syntax-arity ((op (eql 'infinite-annot)) _)
    (declare (ignorable op _))
    :infinite))

(defmacro infinite-annot (&body body)
  `(progn ,@body))

(test test-at-syntax-infinite-annot
  (within-at-syntax-readtable
    (is (equal
         '(infinite-annot 1 2 3 4 5 6 7 8 9)
         '@infinite-annot 1 2 3 4 5 6 7 8 9))
    (is (equal
         '(a b c)
         @infinite-annot
         "a"
         "b"
         "c"
         (list 'a 'b 'c)))
    ;; @(list) syntax does not see the symbol's arity.
    (is (equal
         '(a b c)
         @(infinite-annot
            "a"
            "b"
            "c")
         (list 'a 'b 'c)))))

