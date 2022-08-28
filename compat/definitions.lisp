(in-package #:cl-annot-revisit-compat)

;;; In the original cl-annot, some '@' macros are defined as 'inline'.
;;; I follow the convention below.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +compat-inline-operator-list+
      '(cl-annot-revisit:ignore cl-annot-revisit:ignorable cl-annot-revisit:dynamic-extent
        cl-annot-revisit:special cl-annot-revisit:type cl-annot-revisit:ftype
        cl-annot-revisit:inline cl-annot-revisit:notinline cl-annot-revisit:optimize
        cl-annot-revisit:optional cl-annot-revisit:required)
    :test 'equal)

  ;; - 'declaration' was :inline in cl-annot, but I think it is a bug.
  ;; - 'export' had :alias in cl-annot, but not required on us.

  (defun set-to-compat-inline-operator (operator-name)
    (setf (get operator-name (find-cl-annot-symbol "ANNOTATION-INLINE-P"))
          t))

  (loop for i in +compat-inline-operator-list+
        do (set-to-compat-inline-operator i)))
