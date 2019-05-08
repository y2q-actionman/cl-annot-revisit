(in-package :cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric expand-@export-1 (form-head form)
    (:documentation "Called by `@export' to expand known ones.
If expansion successed, returns (values <expansion> t).
If failed, returns (values <original-form> nil)."))

  (defmethod expand-@export-1 (form-head form)
    "The bottom case, returns (values FORM nil)."
    (declare (ignore form-head))
    (values form nil))

  (defparameter *@export-simple-expansion-symbols*
    '( defclass defconstant define-condition define-method-combination
      define-modify-macro define-symbol-macro deftype defmacro
      defparameter defvar)
    "List of symbols naming a definition form and its
first argument is a name of a object to be defined.")
  
  (defparameter *@export-defun-expansion-symbols*
    '( defun defgeneric define-compiler-macro defmethod)
    "List of symbols naming a definition form and its
first argument is a function name to be defined.")
  
  (defmethod expand-@export-1 ((form-head symbol) form)
    "Called if FORM-HEAD is in `*@export-simple-expansion-symbols*'
or `*@export-defun-expansion-symbols*'."
    (cond ((member form-head *@export-simple-expansion-symbols*)
           (values `(progn (export ',(second form))
                           ,form)
                   t))
          ((member form-head *@export-defun-expansion-symbols*)
           (let* ((function-name (second form))
                  (exported
                   (etypecase function-name
                     (symbol function-name)
                     (list
                      (assert (eq (first function-name) 'cl:setf))
                      (second function-name)))))
             (values `(progn (export ',exported)
                             ,form)
                     t)))
          (t
           (values form nil))))

  (defmethod expand-@export-1 ((form-head (eql 'cl:defstruct)) form)
    "A special handling for `defstruct'."
    ;; TODO: add support for many options?
    (let* ((name-or-options (second form))
           (struct-name
            (etypecase name-or-options
              (symbol name-or-options)
              (list (first name-or-options)))))
      (values `(progn (export ',struct-name)
                      ,form)
              t)))

  ;; Other 'def-' macros in CL:
  ;; - `defsetf' and `define-setf-expander':
  ;;   They requires an opertor name. The name is should be exported there.
  ;; - `defpackage'
  ;;   Package names are string-designeter, not an exported symbol.
  )


(defmacro @export (&body forms &environment env)
  (cond
    ((null forms)
     nil)
    ((not (length= 1 forms))            ; recursive expansion
     `(progn ,@(add-to-all-heads '@export forms)))
    (t
     (let ((form (first forms)))
       (mv-cond-let2 (expansion expanded-p)
         ((expand-@export-1 (first form) form)) ; try known expansions.
         ((apply-to-special-form-1 '@export form)) ; try recursive expansion.
         ((macroexpand-1 form env))             ; try `macroexpand-1'.
         (t                       ; nothing to do. return FORM itself.
          (values form nil)))))))
