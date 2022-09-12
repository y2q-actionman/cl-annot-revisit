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

  (defmethod cl-annot-revisit/at-syntax:expand-at-read-time-p (operator (cl-annot-compatible-p (eql t)))
    (or (get operator 'cl-annot.core:annotation-inline-p)
        (call-next-method)))

  (defmethod cl-annot-revisit/at-syntax:resolve-at-syntax-alias (operator (cl-annot-compatible-p (eql t)))
    (or (get operator 'cl-annot.core:annotation-real)
        (call-next-method))))

;;; In the original cl-annot, some '@' macros are defined as 'inline'.
;;; I follow the convention below.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +compat-inline-operator-list+
      '(cl-annot-revisit:ignore cl-annot-revisit:ignorable cl-annot-revisit:dynamic-extent
        cl-annot-revisit:special cl-annot-revisit:type cl-annot-revisit:ftype
        cl-annot-revisit:inline cl-annot-revisit:notinline cl-annot-revisit:optimize
        ;; 'declaration' was :inline in cl-annot, but I think it is a bug.
        cl-annot-revisit:optional cl-annot-revisit:required)
    :test 'equal)

  (defun set-to-compat-inline-operator (operator-name)
    (setf (get operator-name 'cl-annot.core:annotation-inline-p)
          t))

  (loop for i in +compat-inline-operator-list+
        do (set-to-compat-inline-operator i)))

;;; In the original cl-annot, some macros has an alias from CL symbols.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +compat-alias-operator-list+
      '(cl-annot-revisit:export
        cl-annot-revisit:ignore cl-annot-revisit:ignorable cl-annot-revisit:dynamic-extent
        cl-annot-revisit:special cl-annot-revisit:type cl-annot-revisit:ftype
        cl-annot-revisit:inline cl-annot-revisit:notinline cl-annot-revisit:optimize
        cl-annot-revisit:declaration)
    :test 'equal)

  (defun set-compat-cl-alias-operator (operator-name)
    (let ((cl-symbol (find-symbol (symbol-name operator-name) :cl)))
      (setf (get cl-symbol 'cl-annot.core:annotation-real)
            operator-name)))
  
  (loop for i in +compat-alias-operator-list+
        do (set-compat-cl-alias-operator i)))
