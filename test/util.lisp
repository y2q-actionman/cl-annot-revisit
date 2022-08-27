(in-package #:cl-annot-revisit-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +at-macro-eval-always+
      '(:compile-toplevel :load-toplevel :execute)
    :test 'equal))

(defmacro with-function-aliasing (bindings &body body)
  (loop for (new-name old-name) in bindings
        as args-sym = (gensym "args")
        collect `(,new-name (&rest ,args-sym)
                            (declare (dynamic-extent ,args-sym))
                            (apply #',old-name ,args-sym))
          into clauses
        finally
           (return `(flet ,clauses ,@body))))

(defun equal-ignoring-gensym (x y)
  (flet ((test-fn (x y)
           (or (equal x y)
               (and (symbolp x) (null (symbol-package x))
                    (symbolp y) (null (symbol-package y))))))
    (tree-equal x y) #'test-fn))
