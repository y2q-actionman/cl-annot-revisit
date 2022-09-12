(in-package #:cl-annot-revisit-compat)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lambda-list-required-arguments (lambda-list)
    "Returns list of symbols naming required arguments in LAMBDA-LIST."
    ;; Drop &whole or &environment and its argument.
    (loop for i = (first lambda-list)
          while (member i '(&whole &environment))
          do (setf lambda-list (nthcdr 2 lambda-list)))
    (loop for i in lambda-list
          until (member i lambda-list-keywords)
          collect i))
  ;; TODO: use this and https://github.com/Shinmera/trivial-arguments
  ;; for calc arity in `find-at-syntax-arity'.
  )

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
