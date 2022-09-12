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
       '@cl-annot-revisit:optimize (speed)
       (defgeneric hoge (x))
       '(defgeneric hoge (x)
         (declare (optimize speed)))))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@cl-annot-revisit:optimize")))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@cl-annot-revisit:optimize (speed 3)"))))

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

(test test-at-syntax-symbol-export
  (is (expanded-export-name-equalp
       '@cl-annot-revisit:export
       (defun foo (x) 9999)
       '(foo)))
  (within-at-syntax-readtable
    (signals stream-error
      (read-from-string "@cl-annot-revisit:export"))))

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

(test test-at-syntax-shart-at
  (is (equal
       '#@list 1
       '(list 1)))
  (is (equal
       '#5@list 1 2 3 4 5
       '(list 1 2 3 4 5))))

(defmethod cl-annot-revisit:find-at-syntax-arity ((op (eql 'not-annot-op)) _)
  (declare (ignorable op _))
  nil)

(test test-at-syntax-not-annot
  (within-at-syntax-readtable
    (let ((*at-macro-verbose* t)
          (*package* (find-package :cl-annot-revisit-test)))
      (signals at-macro-style-warning
        (read-from-string "@200"))
      (signals at-macro-style-warning
        (read-from-string "@not-annot-op")))))

(test test-at-syntax-lambda-form
  (is (equal
       '@ (lambda (x y z) (+ x y z)) 1 20 300
       '((lambda (x y z) (+ x y z)) 1 20 300))))

;;; TODO: :infinite
