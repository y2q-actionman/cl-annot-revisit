(in-package #:cl-annot-revisit/at-syntax)

(defgeneric find-at-syntax-arity (operator)
  (:documentation "Returns at-syntax arity of OPERATOR. If this
  returns NIL, OPERATOR is considered as not for @-syntax.
  Default is 1, means OPERATOR takes one argument for '@' syntax.")
  (:method (operator)
    (case operator
      ((cl-annot-revisit:ignore cl-annot-revisit:ignorable cl-annot-revisit:dynamic-extent
         cl-annot-revisit:type cl-annot-revisit:ftype
         cl-annot-revisit:optimize
         cl-annot-revisit:documentation cl-annot-revisit:doc
         cl-annot-revisit:metaclass
         cl-annot-revisit:optional)
       2)
      (otherwise
       1))))

(defgeneric expand-at-read-time-p (operator)
  (:documentation "If this returns T, the macro named OPERATOR will be
  `macroexpand'ed at read-time.

  This feature is for supporting ':inline' feature of the original
  cl-annot, but it is not needed conceptually because you can use '#.'
  anytime.  So, this function is not exported by design.")
  (:method (operator)
    (declare (ignore operator))
    nil))
