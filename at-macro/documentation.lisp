(in-package :cl-annot-revisit/at-macro)

(define-condition @documentation-style-warning (at-macro-style-warning)
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defgeneric insert-documentation-1* (form-operator form docstring)
    (:documentation "Called by `insert-documentation-1' to insert DOCSTRING into FORM.
If FORM can be expanded, returns its expansion. If not, returns nil.")
    )

  (defmethod insert-documentation-1* ((form-operator (eql 'defclass)) form docstring)
    (destructuring-bind (op class-name (&rest superclass-names)
                            (&rest slot-specifiers)
                            &rest options)
        form
      (when (assoc :documentation options)
        (warn '@documentation-style-warning :form form
              :message "Docstring duplicated"))
      `(,op ,class-name (,@superclass-names)
            (,@slot-specifiers)
            (:documentation ,docstring) ,@options)))

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

(defmacro @doc (docstring form &environment env)
  (mv-cond-let2 (expansion expanded-p)
    ((insert-documentation-1 form docstring))
    ;; ((apply-to-special-form-1 `(@doc docstring) form)) ; FIXME: if many forms?
    ((macroexpand-1 form env)
     `(@doc ,docstring ,expansion))
    (t form)))

;;; @documentation は `documentation' かんすう を見る?
