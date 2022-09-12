(in-package :cl-annot-revisit-test)

(named-readtables:in-readtable cl-annot-revisit:at-syntax-readtable)

(test test-compat-read-time-expansion
  (let ((*cl-annot-compatibility* t))
    (is (equal
         (read-from-string
          "(defun hoge (x)
              @cl-annot-revisit:ignorable (x)
              (1+ x))")
         '(defun hoge (x)
           (declare (ignorable x))
           (1+ x))))))

#|
CL-ANNOT-REVISIT/AT-SYNTAX> (count-macro-lambda-list-required-arguments '())
0
CL-ANNOT-REVISIT/AT-SYNTAX> (count-macro-lambda-list-required-arguments '(&whole a))
0
CL-ANNOT-REVISIT/AT-SYNTAX> (count-macro-lambda-list-required-arguments '(&whole a x y z))
3
CL-ANNOT-REVISIT/AT-SYNTAX> (count-macro-lambda-list-required-arguments '(&whole a x y z &key))
3
|#

;; (defannotation @hoge (a) (:alias @fuga)
;;   `(let ((obj ,a))
;;      (pprint obj)
;;      obj))

;; (@hoge 100)
;; (@fuga 100)
