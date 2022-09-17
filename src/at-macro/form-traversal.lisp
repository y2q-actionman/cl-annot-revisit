(in-package #:cl-annot-revisit-at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +standard-variable-definiton-form-list+
      '(cl:defconstant cl:defparameter cl:defvar)
    :test 'equal)
  
  (defgeneric variable-definition-operator-p (operator)
    (:documentation "Returns T if OPERATOR naming a definition form and
its first argument is a variable name to be defined.")
    (:method (operator)
      (member operator +standard-variable-definiton-form-list+)))

  (define-constant +standard-function-definiton-form-list+
      '(cl:defgeneric cl:define-compiler-macro cl:defmethod cl:defun)
    :test 'equal)

  (defgeneric function-definition-operator-p (operator)
    (:documentation "Returns T if OPERATOR naming a definition form and
its first argument is a function name to be defined.")
    (:method (operator)
      (member operator +standard-function-definiton-form-list+)))

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
      "Handles lambda forms. It returns nil."
      (declare (ignorable form-head form))
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

  (defun apply-at-macro-to-special-toplevel-form (at-macro-form form)
    "If form is a special form treated specially at top level
 (`progn', `locally', `macrolet', `symbol-macrolet' and `eval-when'),
 expand FORM into AT-MACRO-FORM recursively."
    ;; When you want to apply other special forms, you should define
    ;; your expanded to do so.
    (unless (consp form)
      (return-from apply-at-macro-to-special-toplevel-form
        (values form nil)))
    (macroexpand-convention (form)
      (destructuring-case form
        ((progn &body body)
         `(progn
            ,@(apply-at-macro-to-multiple-forms at-macro-form body)))
        ((eval-when situations &body body)
         `(eval-when ,situations
            ,@(apply-at-macro-to-multiple-forms at-macro-form body)))
        ((locally &body body)
         (multiple-value-bind (body declarations)
             (parse-body body)
           `(locally ,@declarations
              ,@(apply-at-macro-to-multiple-forms at-macro-form body))))
        (((macrolet symbol-macrolet) bindings &body body)
         (multiple-value-bind (body declarations)
             (parse-body body)
           `(,(first form) ,bindings
             ,@declarations
             ,@(apply-at-macro-to-multiple-forms at-macro-form body))))
        ((otherwise &rest _)
         (declare (ignore _))
         form))))

  (defgeneric allow-macroexpand-internal-form-p (operator)
    (:documentation "Determines whether cl-annot-revisit macros
 try `macroexpand-1' to forms beginning with OPERATOR.
 Default is NIL, because applying `macroexpand-1' change internal forms.
 This may be a problem when `macrolet' was used for for hooking.")
    (:method (_)
      (declare (ignore _))
      nil)
    (:method ((operator symbol))
      "In default, returns T only for our macros."
      (let ((package (symbol-package operator)))
        (or (equal package (find-package :cl-annot-revisit))
            (equal package (find-package :cl-annot-revisit-at-macro))))))

  (defun apply-at-macro-for-each-form (at-macro-form expander-function forms env)
    (declare (ignorable env))
    (cond
      ((null forms)
       (values nil nil))
      ((not (length= 1 forms)) ; If the length is more than 1.
       (values `(progn ,@(apply-at-macro-to-multiple-forms at-macro-form forms)) t))
      (t
       (let ((form (first forms)))
         (mv-cond-let2 (expansion expanded-p)
           ((funcall expander-function form)) ; try known expansions.
           ((apply-at-macro-to-special-toplevel-form at-macro-form form)) ; try recursive expansion.
           ((and (consp form)
                 (allow-macroexpand-internal-form-p (first form))
                 (macroexpand-1 form env)) ; Try `macroexpand-1' only when it is allowed.
            (values `(,@at-macro-form ,expansion) t))
           (t                       ; nothing to be expanded.
            (values form nil))))))))
