(in-package :cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric find-name-to-be-defined* (form-head form)
    (:documentation "Called by `find-name-to-be-defined' to compute a result."))

  (defmethod find-name-to-be-defined* ((form-head list) form)
    "Handling the lambda forms. It returns nil."
    (declare (ignorable form-head form))
    (assert (starts-with 'lambda form-head))
    nil)

  (defparameter *function-definiton-form-list*
    '( defgeneric define-compiler-macro defmethod defun)
    "List of symbols naming a definition form and its
first argument is a function name to be defined.")
  
  (defparameter *standard-definiton-form-list*
    `( defclass defconstant define-condition define-method-combination
       define-modify-macro define-setf-expander define-symbol-macro
       defmacro defpackage defparameter defsetf defstruct deftype defvar
       ,@*function-definiton-form-list*)
    "List of symbols naming a definition form and its
first argument is a name to be defined.")
  
  (defmethod find-name-to-be-defined* ((form-head symbol) form)
    "Called if FORM-HEAD is symbol."
    (if (member form-head *standard-definiton-form-list*)
        (second form)))

  (defmethod find-name-to-be-defined* ((form-head (eql 'cl:defstruct)) form)
    "A special handling for `defstruct'. Its second form may contain some options."
    (let ((name-or-options (second form)))
      (etypecase name-or-options
        (symbol name-or-options)
        (list (first name-or-options)))))

  (defun find-name-to-be-defined (form)
    "If FORM is a form defining something, returns the name to be
defined. Its type depends on FORM. (e.g. may be a List if `defun', A
string-designater if `defpackage'.)
If FORM is not so, returns nil."
    (typecase form
      (symbol nil) ; It may be a symbol macro, so caller must check it.
      (cons (find-name-to-be-defined* (first form) form))
      (otherwise nil))))


(define-condition @export-style-warning (at-macro-style-warning)
  ())


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric expand-@export-1* (form-head form)
    (:documentation "Called by `expand-@export-1' to compute a result.
If FORM can be expanded, returns its expansion. If not, returns nil."))

  (defmethod expand-@export-1* (form-head form)
    "The bottom case. If FORM-HEAD found by `find-name-to-be-defined',
returns the expansion of FORM. If not, returns nil."
    (if-let ((name (find-name-to-be-defined form)))
      (cond ((listp name)
             (unless (and (member form-head *function-definiton-form-list*)
                          (eq (first name) 'cl:setf))
               (warn '@export-style-warning
                     :form form :message "Name ~A looks like non-conforming" name))
             `(progn (@eval-always (export ',(second name))) 
                     ,form))
            (t
             `(progn (@eval-always (export ',name))
                     ,form)))))

  (defun warning-message-on-defsetf-like (operator)
    (format nil "Exporting names in ~A should be placed around its non-setf definition." operator))

  (defmethod expand-@export-1* :before ((form-head (eql 'cl:defsetf)) form)
    (warn '@export-style-warning
          :form form :message (warning-message-on-defsetf-like form-head)))

  (defmethod expand-@export-1* :before ((form-head (eql 'cl:define-setf-expander)) form)
    (warn '@export-style-warning
          :form form :message (warning-message-on-defsetf-like form-head)))

  (defmethod expand-@export-1* ((form-head (eql 'cl:defpackage)) form)
    "A special handling for `defpackage'. It does not define a name as a symbol."
    (warn '@export-style-warning
          :form form :message "@export does not works on DEFPACKAGE.")
    nil)

  (defun expand-@export-1 (form)
    "Called by `@export' to expand known ones.
If expansion successed, returns (values <expansion> t).
If failed, returns (values FORM nil)."
    (typecase form
      (cons (if-let ((expansion (expand-@export-1* (first form) form)))
              (values expansion t)
              (values form nil)))
      ;; If FORM is a symbol, it may be a symbol macro, and it is
      ;; expanded by `@export'.
      (otherwise (values form nil)))))


(defmacro @export (&body forms &environment env)
  (cond
    ((null forms)
     nil)
    ((not (length= 1 forms))            ; recursive expansion
     `(progn ,@(apply-to-all-forms '(@export) forms)))
    (t
     (let ((form (first forms)))
       (mv-cond-let2 (expansion expanded-p)
         ((expand-@export-1 form)) ; try known expansions.
         ((apply-to-special-form-1 '(@export) form)) ; try recursive expansion.
         ((macroexpand-1 form env)      ; try `macroexpand-1'.
          `(@export ,expansion))
         (t                       ; nothing to do. return FORM itself.
          form))))))

;;; TODO: support `restart-case'?
