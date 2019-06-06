(in-package #:cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun apply-to-all-forms (operator-head forms)
    (mapcar (lambda (form) (append operator-head (list form))) forms))

  (defun function-name-p (x)
    (typecase x
      (symbol t)
      (cons (starts-with 'cl:setf x))))

  (defun try-macroexpand (value alt)
    "If VALUE is true, return (values VALUE t). If not, returns (values ALT nil).
This function is intended to follow `macroexpand' convention."
    (if value
        (values value t)
        (values alt nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *variable-definiton-form-list*
    '(defconstant defparameter defvar)
    "List of symbols naming a definition form and its
first argument is a variable name to be defined.")

  (defun variable-definition-operator-p (symbol)
    (member symbol *variable-definiton-form-list*))

  (defparameter *function-definiton-form-list*
    '(defgeneric define-compiler-macro defmethod defun)
    "List of symbols naming a definition form and its
first argument is a function name to be defined.")

  (defun function-definition-operator-p (symbol)
    (member symbol *function-definiton-form-list*))

  (defparameter *definiton-form-list*
    `(defclass define-condition define-method-combination
      define-modify-macro define-setf-expander define-symbol-macro
      defmacro defpackage defsetf defstruct deftype
      ,@*variable-definiton-form-list*
      ,@*function-definiton-form-list*)
    "List of symbols naming a definition form and its
first argument is a name to be defined."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric find-name-to-be-defined* (form-head form)
    (:documentation "Called by `find-name-to-be-defined' to compute a result."))

  (defmethod find-name-to-be-defined* ((form-head list) form)
    "Handling the lambda forms. It returns nil."
    (declare (ignorable form-head form))
    (assert (starts-with 'lambda form-head))
    nil)

  (defmethod find-name-to-be-defined* ((form-head symbol) form)
    "Called if FORM-HEAD is symbol."
    (if (member form-head *definiton-form-list*)
        (second form)))

  ;; special handling for `defstruct' is in 'defstruct.lisp'.

  (defun find-name-to-be-defined (form)
    "If FORM is a form defining something, returns the name to be
defined. Its type depends on FORM. (e.g. may be a List if `defun', A
string-designater if `defpackage'.)
If FORM is not so, returns nil."
    (typecase form
      (symbol nil) ; It may be a symbol macro, so caller must check it.
      (cons (find-name-to-be-defined* (first form) form))
      (otherwise nil))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric apply-to-special-form-1* (at-macro-form form-head form)
    (:documentation
     "An internal function called by `apply-to-special-form-1'")
    (:method (at-macro-form form-head form)
      "Bottom case, returns nil."
      (declare (ignore at-macro-form form-head form))
      nil))

  (defmethod apply-to-special-form-1* (at-macro-form (form-head (eql 'progn)) form)
    `(progn ,@(apply-to-all-forms at-macro-form (rest form))))

  (defmethod apply-to-special-form-1* (at-macro-form (form-head (eql 'eval-when)) form)
    (destructuring-bind ((&rest situations) &body e-w-body) (rest form)
      `(eval-when (,@situations)
         ,@(apply-to-all-forms at-macro-form e-w-body))))

  (defmethod apply-to-special-form-1* (at-macro-form (form-head (eql 'locally)) form)
    (multiple-value-bind (remaining-forms declarations)
        (parse-body (rest form))
      `(locally ,@declarations
         ,@(apply-to-all-forms at-macro-form remaining-forms))))

  (defun apply-to-special-form-1 (at-macro-form form)
    "If form is a special form (one of `progn', `eval-when', or
`locally'), expand FORM into AT-MACRO-FORM recursively."
    (try-macroexpand
     (if (consp form)
         (apply-to-special-form-1* at-macro-form (first form) form))
     form)))

;;; NOTE: At the top level, Common Lisp specially treats `macrolet',
;;; `symbol-macrolet', and *ALL* macro forms. But I need a real code-walker
;;; for supporting them.


;; TODO: rewrite..
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun apply-at-macro (at-macro-form expander-function forms env
                         &key (if-no-expansion #'identity))
    (cond
      ((null forms)
       nil)
      ((not (length= 1 forms))            ; recursive expansion
       `(progn ,@(apply-to-all-forms at-macro-form forms)))
      (t
       (let ((form (first forms)))
         (mv-cond-let2 (expansion expanded-p)
           ((funcall expander-function form)) ; try known expansions.
           ((apply-to-special-form-1 at-macro-form form)) ; try recursive expansion.
           ((macroexpand-1 form env)      ; try `macroexpand-1'.
            `(,@at-macro-form ,expansion))
           (t                       ; nothing to be expanded.
            (funcall if-no-expansion form))))))))
