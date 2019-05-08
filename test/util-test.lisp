(in-package #:cl-annot-revisit-test)

(test expand-recursive-1-progn
  (is (equal (cl-annot-revisit/at-macro::expand-recursively-1
              '@export
              '(progn x y (+ x y)))
             '(PROGN (@EXPORT X) (@EXPORT Y) (@EXPORT (+ X Y))))))

(test expand-recursive-1-eval-when
  (is (equal (cl-annot-revisit/at-macro::expand-recursively-1
              '@export
              '(eval-when (:execute)
                x y (+ x y)))
             '(EVAL-WHEN (:EXECUTE)
               (@EXPORT X) (@EXPORT Y) (@EXPORT (+ X Y))))))

(test expand-recursive-1-locally
  (is (equal (cl-annot-revisit/at-macro::expand-recursively-1
              '@export
              '(locally (declare (type fixnum x))
                x y (+ x y)))
             '(LOCALLY (DECLARE (TYPE FIXNUM X))
               (@EXPORT X) (@EXPORT Y) (@EXPORT (+ X Y))))))

(test expand-recursive-1-symbol-other
  (is (equal (cl-annot-revisit/at-macro::expand-recursively-1
              '@export
              '#1=(let ((x 100))
                    x y (+ x y)))
             '#1#)))
