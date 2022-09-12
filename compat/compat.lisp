(in-package #:cl-annot-revisit-compat)

;;; Copied from the original cl-annot.
(defpackage #:cl-annot.core
  (:use :cl)
  (:export
    #:annotation-real
    #:annotation-arity
    #:annotation-inline-p
    #:annotation-form
    #:annotation-form-p
    #:%annotation))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod cl-annot-revisit/at-syntax:find-at-syntax-arity (operator (cl-annot-compatible-p (eql t)))
    (or (get operator 'cl-annot.core:annotation-arity)
        (call-next-method)))

  (defmethod cl-annot-revisit/at-syntax:eval-at-read-time-p (operator (cl-annot-compatible-p (eql t)))
    (or (get operator 'cl-annot.core:annotation-inline-p)
        (call-next-method)))

  (defmethod cl-annot-revisit/at-syntax:resolve-at-syntax-alias (operator (cl-annot-compatible-p (eql t)))
    (or (get operator 'cl-annot.core:annotation-real)
        (call-next-method))))

;;; In the original cl-annot, some '@' macros are defined as 'inline'
;;; or may have an alias from CL symbols. I follow the convention
;;; below.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun set-cl-annot-compat-config (name arity inline alias-list)
    (setf (get name 'cl-annot.core:annotation-arity) arity
          (get name 'cl-annot.core:annotation-inline-p) inline)
    (loop for alias in alias-list
          do (setf (get alias 'cl-annot.core:annotation-real) name)))

  (set-cl-annot-compat-config 'cl-annot-revisit:doc 2 nil nil)
  (set-cl-annot-compat-config 'cl-annot-revisit:metaclass 2 nil nil)
  (set-cl-annot-compat-config 'cl-annot-revisit:optional 2 t nil)
  (set-cl-annot-compat-config 'cl-annot-revisit:required 1 t nil)
  (set-cl-annot-compat-config 'cl-annot-revisit:export 1 nil '(cl:export))
  (set-cl-annot-compat-config 'cl-annot-revisit:ignore 1 t '(cl:ignore))
  (set-cl-annot-compat-config 'cl-annot-revisit:ignorable 1 t '(cl:ignorable))
  (set-cl-annot-compat-config 'cl-annot-revisit:dynamic-extent 1 t '(cl:dynamic-extent))
  ;; 'declaration' was :inline in cl-annot, but I think it is a bug.
  (set-cl-annot-compat-config 'cl-annot-revisit:declaration 1 nil '(cl:declaration)) ; 
  (set-cl-annot-compat-config 'cl-annot-revisit:special 1 t '(cl:special))
  (set-cl-annot-compat-config 'cl-annot-revisit:type 2 t '(cl:type))
  (set-cl-annot-compat-config 'cl-annot-revisit:ftype 2 t '(cl:ftype))
  (set-cl-annot-compat-config 'cl-annot-revisit:optimize 1 t '(cl:optimize))
  (set-cl-annot-compat-config 'cl-annot-revisit:inline 1 t '(cl:inline))
  (set-cl-annot-compat-config 'cl-annot-revisit:notinline 1 t '(cl:notinline)))



