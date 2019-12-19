(in-package #:cl-annot-revisit/at-syntax)

(defmacro define-at-syntax (name arity &key (inline nil inline-supplied-p))
  "Internal helper"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defmethod find-at-syntax-arity ((_ (eql ',name)))
       (declare (ignorable _))
       ,arity)
     ,@(if inline-supplied-p
           `((defmethod find-at-syntax-inline-p ((_ (eql ',name)))
               (declare (ignorable _))
               ,inline)))))

(define-at-syntax cl-annot-revisit:eval-when-compile 1)
(define-at-syntax cl-annot-revisit:eval-when-load 1)
(define-at-syntax cl-annot-revisit:eval-when-execute 1)
(define-at-syntax cl-annot-revisit:eval-always 1)

(define-at-syntax cl-annot-revisit:add-declaration 1)
(define-at-syntax cl-annot-revisit:ignore 1 :inline t)
(define-at-syntax cl-annot-revisit:ignorable 1 :inline t)
(define-at-syntax cl-annot-revisit:dynamic-extent 1 :inline t)

(define-at-syntax cl-annot-revisit:add-declamation 1)
(define-at-syntax cl-annot-revisit:special 1 :inline t)
(define-at-syntax cl-annot-revisit:type 2 :inline t)
(define-at-syntax cl-annot-revisit:ftype 2 :inline t)
(define-at-syntax cl-annot-revisit:inline 1 :inline t)
(define-at-syntax cl-annot-revisit:notinline 1 :inline t)
(define-at-syntax cl-annot-revisit:optimize 1 :inline t)
(define-at-syntax cl-annot-revisit:declaration 1) ; This is :inline in cl-annot, but not required on us.

(define-at-syntax cl-annot-revisit:documentation 2)
(define-at-syntax cl-annot-revisit:doc 2)

(define-at-syntax cl-annot-revisit:export 1) ; This has :alias in cl-annot, but not required on us.

(define-at-syntax cl-annot-revisit:metaclass 2)
(define-at-syntax cl-annot-revisit:export-slots 1)
(define-at-syntax cl-annot-revisit:export-accessors 1)
(define-at-syntax cl-annot-revisit:export-class 1)
(define-at-syntax cl-annot-revisit:export-constructors 1)
(define-at-syntax cl-annot-revisit:export-structure 1)

(define-at-syntax cl-annot-revisit:optional 2 :inline t)
(define-at-syntax cl-annot-revisit:required 1 :inline t)
