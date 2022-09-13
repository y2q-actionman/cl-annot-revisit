(in-package :cl-annot-revisit-test)

(named-readtables:in-readtable cl-annot-revisit:at-syntax-readtable)

(test test-compat-read-time-eval
  (within-at-syntax-readtable
    (let ((*cl-annot-compatibility* t)
          (*package* (find-package :cl-annot-revisit-test)))
      (is (equal
           (read-from-string
            "(defun hoge (x)
              @cl-annot-revisit:ignorable (x)
              (1+ x))")
           '(defun hoge (x)
             (declare (ignorable x))
             (1+ x)))))))

(test test-compat-alias
  (within-at-syntax-readtable
    (let ((*cl-annot-compatibility* t)
          (*package* (find-package :cl-annot-revisit-test)))
      (is (equal
           (read-from-string
            "(defun fuga (x)
              @optimize (speed safety)
              (1+ x))")
           '(defun fuga (x)
             (declare (optimize speed safety))
             (1+ x)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-annot-revisit-compat:defannotation hoge-annot (a)
      (:inline nil :alias nil)
    `(let ((obj ,a))
       (pprint obj)
       obj))

  (cl-annot-revisit-compat:defannotation fuga-annot (a b c)
      (:inline nil :alias piyo-annot)
    `(cl-annot-revisit:eval-always ,a ,b ,c))

  (cl-annot-revisit-compat:defannotation read-time-eval-annot (a)
      (:inline t)
    `(format nil "~A is ~A" ',a ,a)))

(test test-compat-defannotation
  (is (equal-after-macroexpand
       '@hoge-annot #1=(lambda (x) (+ 1 2 3))
       '(let ((obj #1#))
         (pprint obj)
         obj)))
  (is (equal-after-macroexpand
       '@fuga-annot 100 200 300
       '(eval-when (:compile-toplevel :load-toplevel :execute)
         100 200 300)))
  (is (equal-after-macroexpand
       '@read-time-eval-annot (+ 1 2 3)
       (format nil "~A is ~A" '(+ 1 2 3) (+ 1 2 3)))))
