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
                               alias inline)
                         &body body)
  (check-type name symbol)
  (unless (starts-with #\@ (symbol-name name))
    (warn "Name for defannotation should be started with @"))
  (let ((alias-form
         (when alias
           ;; (warn ":alias keyword is not recommended.")
           `(defannotation ,alias (&rest forms) (:arity ,arity :inline ,inline)
              `(,',name ,@forms)))))
    `(progn
       (setf (get 'name 'arity) ,arity)
       ,@(if inline
             `((setf (get 'name 'inline-p) ,inline)))
       ,@(if alias-form
             `(,alias-form))
       (defmacro ,name ,lambda-list
         ,@body))))
