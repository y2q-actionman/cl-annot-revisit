(in-package #:cl-annot-revisit-compat)

(defmacro defannotation
    (name lambda-list
     (&key (arity (length (lambda-list-required-arguments lambda-list)))
        (inline nil inline-supplied-p)
        alias)
     &body body)
  "`cl-annot:defannotation' like one."
  (let ((alias-form
          (when alias
            `(defmethod cl-annot-revisit/at-syntax:resolve-at-syntax-alias ((_ (eql ',alias)) _bool)
               (declare (ignorable _ _bool))
               ',name)))
        (inline-form
          (when inline-supplied-p
            `(defmethod cl-annot-revisit/at-syntax::eval-at-read-time-p ((_ (eql ',name)) _bool)
               (declare (ignorable _ _bool))
               ,inline))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defmethod find-at-syntax-arity ((_ (eql ',name)) _bool)
           (declare (ignorable _ _bool))
           ,arity)
         ,@(if inline-form
               `(,inline-form))
         ,@(if alias-form
               `(,alias-form)))
       (defmacro ,name ,lambda-list
         ,@body))))
