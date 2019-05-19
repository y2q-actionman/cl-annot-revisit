(in-package :cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defgeneric insert-documentation-1* (form-head form docstring)
    (:documentation "Called by `insert-documentation-1' to insert DOCSTRING into FORM.
If FORM can be expanded, returns its expansion. If not, returns nil.")
    (:method (form-head form docstring)
      "The bottom case, returns nil."
      (declare (ignore form-head form docstring))
      nil))

  ;; TODO
  ;; (defclass defconstant defgeneric define-compiler-macro define-condition
  ;;         define-method-combination define-modify-macro
  ;;         define-setf-expander define-symbol-macro defmacro defmethod
  ;;         defpackage defparameter defsetf defstruct deftype defun
  ;;         defvar)


  ;; TODO: say warnings about local declarations:
  ;; about `flet', `labels', `macrolet'.

  ;; TODO: lambda

  ;; TODO: (function (lambda ...)) ??



  (defun insert-documentation-1 (form docstring)
    "Insert DOCSTRING into FORM.
If insertion successed, returns (values <expansion> t).
If failed, returns (values <original-form> nil)."
    (typecase form
      (cons
       (if-let ((expansion (insert-documentation-1* (first form) form docstring)))
         (values expansion t)
         (values form nil)))
      (otherwise (values form nil)))))

;;; @documentation は `documentation' かんすう を見る?
