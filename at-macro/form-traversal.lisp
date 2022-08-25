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
              (variable-definition-operator-p form-head) ; Calls it because defmethod may exist.
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


  (defun apply-at-macro-to-all-forms (at-macro-form forms)
    (values
     (loop for form in forms
           collect `(,@at-macro-form ,form))
     t))                                ; `macroexpand' convention.

  (defun apply-at-macro-to-special-form (at-macro-form form)
    "If form is a special form (one of `progn', `eval-when', or
`locally'), expand FORM into AT-MACRO-FORM recursively."
    (macroexpand-convention (form)
      (if (not (consp form))
          form
          (case (first form)
            ((progn)
             `(progn
                ,@(apply-at-macro-to-all-forms at-macro-form (rest form))))
            ((eval-when)
             (let ((eval-when-situations (second form))
                   (eval-when-body (nthcdr 2 form)))
               `(eval-when (,@eval-when-situations)
                  ,@(apply-at-macro-to-all-forms at-macro-form eval-when-body))))
            ((locally)
             (multiple-value-bind (remaining-forms declarations)
                 (parse-body (rest form))
               `(locally ,@declarations
                  ,@(apply-at-macro-to-all-forms at-macro-form remaining-forms))))
            (otherwise
             form))))))

;;; NOTE: At the top level, Common Lisp specially treats `macrolet',
;;; `symbol-macrolet', and *ALL* macro forms. But I need a real code-walker
;;; for supporting them.


;; TODO: rewrite..
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun apply-at-macro (at-macro-form expander-function forms env)
    (cond
      ((null forms)
       (values nil nil))
      ((not (length= 1 forms))            ; recursive expansion
       (values
        `(progn ,@(apply-at-macro-to-all-forms at-macro-form forms))
        t))
      (t
       (let ((form (first forms)))
         (mv-cond-let2 (expansion expanded-p)
           ((funcall expander-function form)) ; try known expansions.
           ((apply-at-macro-to-special-form at-macro-form form)) ; try recursive expansion.
           ((macroexpand-1 form env)      ; try `macroexpand-1'.
            (values `(,@at-macro-form ,expansion) t))
           (t                       ; nothing to be expanded.
            (values form nil))))))))
