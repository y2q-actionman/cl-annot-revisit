(in-package #:cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun add-export (names form)
    (check-type names list)
    (if names
        `(progn (eval-when (:compile-toplevel :load-toplevel :execute)
                  (export ',names))
                ,form)
        form))
  
  (defgeneric expand-export-form-using-head (form-head form)
    (:documentation "Called by `expand-export-form' to compute a result.
If FORM can be expanded, returns its expansion. If not, returns FORM.")
    (:method (form-head form)
      "General case. If FORM found by `find-name-to-be-defined',
returns the expansion of FORM. If not, returns FORM."
      (declare (ignore form-head))
      (if-let ((name (find-name-to-be-defined form)))
        (cond ((listp name)
               (when (and *at-macro-verbose*
                          (not (function-name-p name)))
                 (warn 'at-macro-style-warning
                       :form form :message "Name ~A looks like non-conforming" name))
               (let ((setf-funcion-symbol (second name)))
                 (add-export (list setf-funcion-symbol) form)))
              (t
               (add-export (list name) form)))
        form))
    (:method ((form-head (eql 'cl:defpackage)) form)
      "A special handling for `defpackage'. It does not define a name as a symbol."
      (when *at-macro-verbose*
        (warn 'at-macro-style-warning
              :form form :message "cl-annot-revisit:export does not works on DEFPACKAGE."))
      form))

  (defun expand-export-form (form)
    "Called by `cl-annot-revisit:export' to expand known ones.
If expansion successed, returns (values <expansion> t).
If failed, returns (values FORM nil)."
    (macroexpand-convention (form)
      (if (consp form)
          (expand-export-form-using-head (first form) form)
          form))))

(defmacro cl-annot-revisit:export (&body forms &environment env)
  "`export' the defined names in FORMS."
  (apply-at-macro-for-each-form '(cl-annot-revisit:export) #'expand-export-form forms env))

;;; TODO: support `restart-case'?
