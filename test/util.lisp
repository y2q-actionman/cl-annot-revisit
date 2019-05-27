(in-package #:cl-annot-revisit-test)

(defmacro with-interpackage-falias ((&rest names) package-designator &body body)
  (loop with package = (find-package package-designator)
     for name in names
     as sym = (find-symbol (string name) package)
     collect `(,name (&rest args) (apply #',sym args)) into clauses
     finally
       (return `(flet ,clauses ,@body))))

