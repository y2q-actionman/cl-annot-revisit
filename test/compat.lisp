(in-package :cl-annot-revisit-test)

(named-readtables:in-readtable cl-annot-revisit:at-syntax-readtable)

(defmacro with-compat-testing (&body body)
  `(within-at-syntax-readtable
     (let ((*cl-annot-compatibility* t)
           (*package* (find-package :cl-annot-revisit-test)))
       ,@body)))

(test test-compat-alias
  (with-compat-testing
    (is (equal-after-macroexpand
         (read-from-string
          "@cl:export (defun piyo ())")
         '(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (export '(piyo)))
           (defun piyo ()))))))

(test test-compat-read-time-expansion
  (with-compat-testing
    (is (equal
         (read-from-string
          "(defclass foo ()
              (slot0
               @cl-annot-revisit:optional initform1 slot1
               @cl-annot-revisit:optional initform2 (slot2 :initarg xxx)
               @cl-annot-revisit:optional initform3 (slot3 :initarg yyy :initform zzz)))")
         '(defclass foo ()
           (slot0
            (slot1 :initform initform1 :initarg :slot1)
            (slot2 :initform initform2 :initarg xxx)
            (slot3 :initarg yyy :initform zzz)))))
    (let* ((bar-defclass
             (read-from-string
              "(defclass bar () (@cl-annot-revisit:required slot0))"))
           (bar-defclass-slot1
             (first (fourth bar-defclass))))
      (is (starts-with-subseq '(defclass bar ()) bar-defclass))
      (is (and (starts-with 'slot0 bar-defclass-slot1)
               (eq (getf (rest bar-defclass-slot1) :initarg) :slot0)
               (getf (rest bar-defclass-slot1) :initform))))))

(test test-compat-declarations
  (with-compat-testing
    (is (equal
         (read-from-string
          "(defun hoge (x)
            @cl-annot-revisit:ignore x
            )")
         '(defun hoge (x)
           (declare (ignore x)))))
    (is (equal
         (read-from-string
          "(defun hoge (x)
            @cl:ignorable (x)
            (1+ x))")
         '(defun hoge (x)
           (declare (ignorable x))
           (1+ x))))
    (is (equal
         (read-from-string
          "(defun hoge (x)
            @dynamic-extent (x y z)
            (1+ x))")
         '(defun hoge (x)
           (declare (dynamic-extent x y z))
           (1+ x))))
    (is (equal
         (read-from-string
          "(defun hoge (*x*)
            @special (*x*)
            (+ *x* *x*))")
         '(defun hoge (*x*)
           (declare (special *x*))
           (+ *x* *x*))))
    (is (equal
         (read-from-string
          "(defun hoge (x)
            @type integer x
            @ftype (function (t) t) func
            x)")
         '(defun hoge (x)
           (declare (type integer x))
           (declare (ftype (function (t) t) func))
           x)))
    (is (equal
         (read-from-string
          "(defun hoge (x)
            @inline foo
            @notinline bar
            x)")
         '(defun hoge (x)
           (declare (inline foo))
           (declare (notinline bar))
           x)))
    (is (equal
         (read-from-string
          "(defun fuga (x)
            @optimize (speed safety)
            @optimize (debug 0)
            @optimize ((compilation-speed 1))
            (1+ x))")
         '(defun fuga (x)
           (declare (optimize speed safety))
           (declare (optimize (debug 0)))
           (declare (optimize (compilation-speed 1)))
           (1+ x))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-annot-revisit-compat:defannotation hoge-annot (a)
      (:inline nil :alias nil)
    `(let ((obj ,a))
       (pprint obj)
       obj))

  (cl-annot-revisit-compat:defannotation fuga-annot (a b c)
      (:inline nil :alias piyo-annot)
    `(cl-annot-revisit:eval-always ,a ,b ,c))

  (cl-annot-revisit-compat:defannotation read-time-expand-annot (a)
      (:inline t)
    `(format nil "~A is ~A" ',a ,a)))

(test test-compat-defannotation
  (with-compat-testing
    (is (equal-after-macroexpand
         (read-from-string
          "@hoge-annot (lambda (x) (+ 1 2 3))")
         '(let ((obj (lambda (x) (+ 1 2 3))))
           (pprint obj)
           obj)))
    (is (equal-after-macroexpand
         (read-from-string
          "@fuga-annot 100 200 300")
         '(eval-when (:compile-toplevel :load-toplevel :execute)
           100 200 300)))
    (is (equal-after-macroexpand
         (read-from-string
          "@read-time-expand-annot (+ 1 2 3)")
         '(format nil "~A is ~A" '(+ 1 2 3) (+ 1 2 3))))))
