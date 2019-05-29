(in-package #:cl-annot-revisit-test)

(defmacro with-interpackage-falias ((&rest names) package-designator &body body)
  (loop with package = (find-package package-designator)
     for name in names
     as sym = (find-symbol (string name) package)
     collect `(,name (&rest args) (apply #',sym args)) into clauses
     finally
       (return `(flet ,clauses ,@body))))

(defun equal-ignoring-gensym (x y)
  (flet ((test-fn (x y)
           (or (equal x y)
               (and (symbolp x) (null (symbol-package x))
                    (symbolp y) (null (symbol-package y))))))
    (tree-equal x y) #'test-fn))
