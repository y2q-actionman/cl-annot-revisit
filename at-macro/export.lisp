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
returns the expansion of FORM. If not, returns nil."
      (if-let ((name (find-name-to-be-defined form)))
        (cond ((listp name)
               (unless (and (function-definition-operator-p form-head)
                            (function-name-p name))
                 (warn 'at-macro-style-warning
                       :form form :message "Name ~A looks like non-conforming" name))
               (add-export (list (second name)) form))
              (t
               (add-export (list name) form)))
        form)))

  (defun warn-around-defsetf-like (operator form)
    (when *at-macro-verbose*
      (warn 'at-macro-style-warning :form form
            :message (format nil "Exporting names in ~A should be placed around its non-setf definition."
                             operator))))

  (defmethod expand-export-form-using-head :before ((form-head (eql 'cl:defsetf)) form)
    (warn-around-defsetf-like form-head form))

  (defmethod expand-export-form-using-head :before ((form-head (eql 'cl:define-setf-expander)) form)
    (warn-around-defsetf-like form-head form))

  (defmethod expand-export-form-using-head ((form-head (eql 'cl:defpackage)) form)
    "A special handling for `defpackage'. It does not define a name as a symbol."
    (when *at-macro-verbose*
      (warn 'at-macro-style-warning
            :form form :message "cl-annot-revisit:export does not works on DEFPACKAGE."))
    form)

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
  (apply-at-macro '(cl-annot-revisit:export) #'expand-export-form forms env))

;;; TODO: support `restart-case'?
