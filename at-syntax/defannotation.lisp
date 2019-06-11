(in-package #:cl-annot-revisit/at-syntax)

(defun lambda-list-required-arguments (lambda-list)
  ;; Drop &whole or &environment and its argument.
  (loop for i = (first lambda-list)
     while (member i '(&whole &environment))
     do (setf lambda-list (nthcdr 2 lambda-list)))
  (loop for i in lambda-list
     until (member i lambda-list-keywords)
     collect i))

(defmacro defannotation (name lambda-list
                         (&key (arity (length (lambda-list-required-arguments lambda-list)))
                               (inline nil inline-supplied-p)
                               alias)
                         &body body)
  (check-type name symbol)
  (when (and *at-macro-verbose*
             (not (starts-with #\@ (symbol-name name))))
    (warn "Name for defannotation should be started with @."))
  (unless (or (integerp arity)
              (eq arity :infinite))
    (cerror "Use value."
            ":arity must be an integer or ':infinite'."))
  (let ((alias-form
         (when alias
           (when *at-macro-verbose*
             (warn ":alias keyword is not recommended."))
           `(defannotation ,alias (&rest forms) (:arity ,arity :inline ,inline)
              `(,',name ,@forms)))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defmethod find-at-syntax-arity ((_ (eql ',name)))
           (declare (ignore _))
           ,arity)
         ,@(if inline-supplied-p
               `((defmethod find-at-syntax-inline-p ((_ (eql ',name)))
                   (declare (ignore _))
                   ,inline))))
       ,@(if alias-form
             `(,alias-form))
       (defmacro ,name ,lambda-list
         ,@body))))
