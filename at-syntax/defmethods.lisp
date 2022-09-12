(in-package #:cl-annot-revisit/at-syntax)

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

(defgeneric find-at-syntax-arity (operator cl-annot-compatible-p)
  (:documentation "Returns at-syntax arity of OPERATOR. If this
  returns NIL, OPERATOR is considered as not for @-syntax.
  Default is 1, means OPERATOR takes one argument for '@' syntax.")
  (:method (operator cl-annot-compatible-p)
    (declare (ignore cl-annot-compatible-p))
    (typecase operator
      (symbol
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
      (list
       (destructuring-case operator
         ((lambda lambda-list &body body)
          (declare (ignore body))
          (length (lambda-list-required-arguments lambda-list)))
         ((otherwise &rest _)
          (declare (ignore _))
          nil)))
      (t nil))))

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
