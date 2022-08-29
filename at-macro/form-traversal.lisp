(in-package #:cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +standard-variable-definiton-form-list+
      '(cl:defconstant cl:defparameter cl:defvar)
    :test 'equal)
  
  (defgeneric variable-definition-operator-p (symbol)
    (:documentation "Returns T if SYMBOL naming a definition form and
its first argument is a variable name to be defined.")
    (:method (_)
      (declare (ignore _))
      nil)
    (:method ((symbol symbol))
      (member symbol +standard-variable-definiton-form-list+)))

  (define-constant +standard-function-definiton-form-list+
      '(cl:defgeneric cl:define-compiler-macro cl:defmethod cl:defun)
    :test 'equal)

  (defgeneric function-definition-operator-p (symbol)
    (:documentation "Returns T if SYMBOL naming a definition form and
its first argument is a function name to be defined.")
    (:method (_)
      (declare (ignore _))
      nil)
    (:method ((symbol symbol))
      (member symbol +standard-function-definiton-form-list+)))

  (define-constant +standard-definiton-form-list+
      (append '(cl:defclass cl:define-condition cl:define-method-combination
                cl:define-modify-macro cl:define-setf-expander cl:define-symbol-macro
                cl:defmacro cl:defpackage cl:defsetf cl:defstruct cl:deftype)
              +standard-variable-definiton-form-list+
              +standard-function-definiton-form-list+)
    :test 'equal)

  (defgeneric find-name-to-be-defined-using-head (form-head form)
    (:documentation "Called by `find-name-to-be-defined' to compute a result.")
    (:method ((form-head list) form)
      "Handling the lambda forms. It returns nil."
      (declare (ignorable form-head form))
      (assert (starts-with 'lambda form-head))
      nil)
    (:method ((form-head symbol) form)
      "Called if FORM-HEAD is symbol."
      (if (or (member form-head +standard-definiton-form-list+)
              ;; Calls them because user-defined defmethods may exist.
              (variable-definition-operator-p form-head)
              (function-definition-operator-p form-head))
          (second form))))
  
  ;; special handling for `defstruct' is in 'defstruct.lisp'.

  (defun find-name-to-be-defined (form)
    "If FORM is defining something (like `defun', `defpackage', etc),
returns the name to be defined. If not, returns nil."
    (typecase form
      (symbol nil) ; It may be a symbol macro. Callers must check it.
      (cons (find-name-to-be-defined-using-head (first form) form))
      (otherwise nil)))


  (defun apply-at-macro-to-multiple-forms (at-macro-form forms) ; TODO: rename
    (loop for form in forms
          collect `(,@at-macro-form ,form)))

  (defun apply-at-macro-to-special-form (at-macro-form form)
    "If form is a special form (one of `progn', `eval-when', or
`locally'), expand FORM into AT-MACRO-FORM recursively."
    (check-type form cons)
    (macroexpand-convention (form)
      (destructuring-case form
        (((progn
            catch multiple-value-call multiple-value-prog1 unwind-protect ; evaluates the first arg and body.
            progv)              ; `progv' evaluates two args and body.
          &body body)
         `(,(first form) ,@(apply-at-macro-to-multiple-forms at-macro-form body)))
        (((block eval-when) arg1 &body body) ; Do not evaluate the first value.
         `(,(first form) ,arg1 ,@(apply-at-macro-to-multiple-forms at-macro-form body)))

        ;; Takes declarations and forms.
        ((locally &body body)
         (multiple-value-bind (body declarations)
             (parse-body body)
           `(locally ,@declarations
              ,@(apply-at-macro-to-multiple-forms at-macro-form body))))
        (((flet labals macrolet let let* symbol-macrolet) bindings &body body)
         (multiple-value-bind (body declarations)
             (parse-body body)
           `(,(first form) ,bindings
             ,@declarations
             ,@(apply-at-macro-to-multiple-forms at-macro-form body))))
        
        ;; Takes fixed length forms.
        (((go function quote) _)
         (declare (ignore _))
         form)
        (((return-from the) arg1 &optional arg2) ; `the' requires two args, but I merged it to this path.
         `(,(first form) ,arg1 (,@at-macro-form ,arg2)))
        ((load-time-value arg1 &optional read-only-p)
         `(load-time-value (,@at-macro-form ,arg1) ,read-only-p))
        ((throw tag result)
         `(throw (,@at-macro-form ,tag) ; `throw' evaluates tag.
            (,@at-macro-form ,result)))

        ((if test-form then-form &optional else-form)
         ;; This is very weird... Does anyone want this?
         `(if (,@at-macro-form ,test-form)
              (,@at-macro-form ,then-form)
              (,@at-macro-form ,else-form)))

        ((setq &rest args)
         (error "FIXME"))
        
        ((tagbody &rest args)
         (error "FIXME"))
        
        ((otherwise &rest _)            ; Not special forms
         (declare (ignore _))
         form)))))

;;; NOTE: At the top level, Common Lisp specially treats `macrolet',
;;; `symbol-macrolet', and *ALL* macro forms. But I need a real code-walker
;;; for supporting them.


;; TODO: rewrite..
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun apply-at-macro (at-macro-form expander-function forms env)
    (cond
      ((null forms)
       (values nil nil))
      ((not (length= 1 forms)) ; If the length is more than 1.
       ;; Wraps the contents with `progn'. It will be expanded after.
       (values `(,@at-macro-form (progn ,@forms)) t))
      (t
       (let ((form (first forms)))
         (mv-cond-let2 (expansion expanded-p)
           ((funcall expander-function form)) ; try known expansions.
           ((apply-at-macro-to-special-form at-macro-form form)) ; try recursive expansion.
           ;; TODO: try lambda-form
           ((macroexpand-1 form env)      ; try `macroexpand-1'.
            (values `(,@at-macro-form ,expansion) t))
           (t                       ; nothing to be expanded.
            (values form nil))))))))
