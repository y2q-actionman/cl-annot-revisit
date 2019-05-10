(in-package :cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric find-name-to-be-defined* (form-head form)
    (:documentation "Called by `find-name-to-be-defined' to compute a result."))

  (defmethod find-name-to-be-defined* ((form-head list) form)
    "Handling the lambda forms. It returns nil."
    (declare (ignorable form-head form))
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
        (second form)
        nil))

  (defmethod find-name-to-be-defined* ((form-head (eql 'cl:defstruct)) form)
    "A special handling for `defstruct'. Its second form may contain some options."
    ;; TODO: add support for many options?
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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric expand-@export-1* (form-head form)
    (:documentation "Called by `expand-@export-1' to compute a result."))

  (defmethod expand-@export-1* (form-head form)
    "The bottom case. If a name found by `find-name-to-be-defined',
returns (values <expansion> t). If not, returns (values FORM nil)."
    (if-let ((name (find-name-to-be-defined form)))
      (cond ((listp name)
             (unless (and (member form-head *function-definiton-form-list*)
                          (eq (first name) 'cl:setf))
               ;; TODO: use our style-warning.
               (warn "Name ~A looks not-conforming, form is ~A"
                     name form))
             (values `(progn (@eval-always (export ',(second name))) 
                             ,form)
                     t))
            (t
             (values `(progn (@eval-always (export ',name))
                             ,form)
                     t)))
      (values form nil)))

  (define-condition @export-defsetf-style-warning (at-macro-style-warning)
    ((operator-name :initarg :operator-name :initform "(not supplied"))
    (:report
     (lambda (condition stream)
       (format stream "Exporting names in ~A should be placed around its non-setf definition."
               (slot-value 'operator-name condition)))))

  (defmethod expand-@export-1* :before ((form-head (eql 'cl:defsetf)) form)
    "Say a warning about `defsetf'."
    (declare (ignorable form))
    (warn '@export-defsetf-style-warning :operator-name form-head))

  (defmethod expand-@export-1* :before ((form-head (eql 'cl:define-setf-expander)) form)
    "Say a warning about `define-setf-expander'."
    (declare (ignorable form))
    (warn '@export-defsetf-style-warning :operator-name form-head))

  (defmethod expand-@export-1* ((form-head (eql 'cl:defpackage)) form)
    "A special handling for `defpackage'. It does not define a name as a symbol."
    (values form nil))

  (defun expand-@export-1 (form)
    "Called by `@export' to expand known ones.
If expansion successed, returns (values <expansion> t).
If failed, returns (values FORM nil)."
    (typecase form
      (symbol (values form nil)) ; It may be a symbol macro, it is called by the caller.
      (cons (expand-@export-1* (first form) form))
      (otherwise (values form nil)))))


(defmacro @export (&body forms &environment env)
  (cond
    ((null forms)
     nil)
    ((not (length= 1 forms))            ; recursive expansion
     `(progn ,@(add-to-all-heads '@export forms)))
    (t
     (let ((form (first forms)))
       (mv-cond-let2 (expansion expanded-p)
         ((expand-@export-1 form)) ; try known expansions.
         ((apply-to-special-form-1 '@export form)) ; try recursive expansion.
         ((macroexpand-1 form env))             ; try `macroexpand-1'.
         (t                       ; nothing to do. return FORM itself.
          (values form nil)))))))

;;; TODO: support `restart-case'?
