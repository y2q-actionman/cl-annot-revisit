(in-package #:cl-annot-revisit/at-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun count-lambda-list-required-arguments (lambda-list)
    "Counts required arguments in LAMBDA-LIST."
    (let ((required-arg-start lambda-list))
      (when (starts-with '&whole required-arg-start)
        (setf required-arg-start (nthcdr 2 required-arg-start)))
      (when (starts-with '&environment required-arg-start)
        (setf required-arg-start (nthcdr 2 required-arg-start)))
      (loop for i in required-arg-start
            until (member i lambda-list-keywords)
            count i))))

(defgeneric find-at-syntax-arity (operator cl-annot-compatible-p)
  (:documentation "Returns at-syntax arity of OPERATOR. If this
  returns NIL, OPERATOR is considered as not for @-syntax.
  Default is 1, means OPERATOR takes one argument for '@' syntax.")
  (:method ((operator symbol) cl-annot-compatible-p)
    (declare (ignore cl-annot-compatible-p))
    (case operator
      ((cl-annot-revisit:ignore cl-annot-revisit:ignorable cl-annot-revisit:dynamic-extent
         cl-annot-revisit:type cl-annot-revisit:ftype
         cl-annot-revisit:optimize
         cl-annot-revisit:documentation cl-annot-revisit:doc
         cl-annot-revisit:metaclass
         cl-annot-revisit:optional)
       2)
      (otherwise
       1)))
  (:method ((operator cons) cl-annot-compatible-p)
    (declare (ignore cl-annot-compatible-p))
    (if (lambda-expression-p operator)
        (count-lambda-list-required-arguments (second operator))
        1))
  (:method (operator cl-annot-compatible-p)
    (declare (ignore operator cl-annot-compatible-p))
    nil))

(defgeneric eval-at-read-time-p (operator cl-annot-compatible-p)
  (:documentation "If this returns T, the macro named OPERATOR will be
  `eval'ed at read-time.

  This feature is for supporting ':inline' feature of the original
  cl-annot, but it is not needed conceptually because you can use '#.'
  anytime.")
  (:method (operator cl-annot-compatible-p)
    (declare (ignore operator cl-annot-compatible-p))
    nil))

(defgeneric resolve-at-syntax-alias (operator cl-annot-compatible-p)
  (:documentation "This functions is for supporting ':alias' feature
  of the original cl-annot. It was used for translating CL symbols to
  CL-ANNOT symbols implicitly.
  (E.g. \"@export\" -> `cl:export' -> `cl-annot.std:export*')

  By default, this function does nothing; returns OPERATOR as-is.")
  (:method (operator cl-annot-compatible-p)
    (declare (ignore cl-annot-compatible-p))
    operator))
