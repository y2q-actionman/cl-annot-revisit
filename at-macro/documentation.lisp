(in-package #:cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric operator-doc-type (symbol)
    (:documentation "Returns the doc-type (in `cl:documentation') of SYMBOL.")
    (:method (_)
      (declare (ignore _))
      nil)
    (:method ((symbol symbol))
      (case symbol
        ((cl:defclass cl:define-condition cl:deftype)
         'cl:type)
        ((cl:defconstant cl:defparameter cl:defvar)
         'cl:variable)
        ((cl:defgeneric cl:defmacro cl:defun
           cl:define-modify-macro) ; define-modify-macro == defmacro == `function'
         'cl:function)
        ((cl:define-compiler-macro)
         'cl:compiler-macro)
        ((cl:define-method-combination)
         'cl:method-combination)
        ((cl:define-setf-expander cl:defsetf)
         'cl:setf)
        ((cl:defmethod cl:defpackage)
         t)
        ((cl:defstruct)            ; `defstruct' is specially treated.
         '(cl:structure cl:type))
        ;; There is no docstring for `define-symbol-macro'.
        (otherwise
         nil))))
  
  (defun warn-around-defclass (operator form)
    (when *at-macro-verbose*
      (warn 'at-macro-style-warning :form form
            :message (format nil "Adding documentation into ~A form does not works for slots."
                             operator))))
  
  (defun warn-around-local-form (operator form)
    (when *at-macro-verbose*
      (warn 'at-macro-style-warning :form form
            :message (format nil "Adding declarations into ~A form doesn't work for local docstrings."
                             operator))))
  
  (defgeneric expand-documentation-using-head (operator docstring form)
    (:documentation "Called by `expand-documentation' to insert DOCSTRING into FORM.
If FORM can be expanded, returns its expansion. If not, returns FORM.")
    (:method (operator docstring form)
      "General case."
      (if-let ((doc-type (operator-doc-type operator)))
        ;; Using the result of FORM is simple, but may prevent compilation as a top-level form.
        (with-gensyms (obj)
          `(let ((,obj ,form))
             (setf (documentation ,obj ',doc-type) ,docstring)
             ,obj))
        form))
    (:method :before ((operator (eql 'defclass)) docstring form)
      (declare (ignore docstring))
      (warn-around-defclass operator form))
    (:method :before ((operator (eql 'define-condition)) docstring form)
      (declare (ignore docstring))
      (warn-around-defclass operator form))
    (:method :before ((operator (eql 'flet)) docstring form)
      (declare (ignore docstring))
      (warn-around-local-form operator form))
    (:method :before ((operator (eql 'labels)) docstring form)
      (declare (ignore docstring))
      (warn-around-local-form operator form))
    (:method :before ((operator (eql 'macrolet)) docstring form)
      (declare (ignore docstring))
      (warn-around-local-form operator form))
    (:method ((operator (eql 'lambda)) docstring form)
      "Special handling for `lambda', adds docstring to an anonymous function."
      (destructuring-bind (op lambda-list &rest body0) form
        (multiple-value-bind (body decls old-doc)
            (parse-body body0 :documentation t :whole form)
          (when (and old-doc docstring)
            (error 'at-macro-error :form form
                                   :message "Lambda form already has a docstring."))
          (let* ((new-doc (or docstring old-doc))
                 (new-body (or body
                               (if new-doc `(nil) nil)))) ; Handling no body ; (lambda ())
            `(,op ,lambda-list ,@decls
                  ,@(ensure-list new-doc)
                  ,@new-body)))))
    (:method ((operator (eql 'function)) docstring form)
      "Special handling for #'(lambda ..), adds docstring to an anonymous function."
      (if (starts-with 'lambda (second form))
          `(function ,(expand-documentation-using-head 'lambda docstring (second form)))
          form)))

  ;; special handling for `defstruct' is in 'defstruct.lisp'.

  (defun expand-documentation (docstring form)
    "Insert DOCSTRING into FORM.
If insertion successed, returns (values <expansion> t).
If failed, returns (values <original-form> nil)."
    (macroexpand-convention (form)
     (if (consp form)
         (expand-documentation-using-head (first form) docstring form)
         form))))

(defmacro cl-annot-revisit:documentation (docstring &body forms &environment env)
  "Insert DOCSTRING into FORMS."
  ;; Should I warn about 'setting same docstrings into many forms'?
  (apply-at-macro `(cl-annot-revisit:documentation ,docstring)
                  (lambda (form) (expand-documentation docstring form)) 
                  forms env))

(defmacro cl-annot-revisit:doc (docstring form)
  "Just an alias of (cl-annot-revisit:documentation ...)"
  `(cl-annot-revisit:documentation ,docstring ,form))
