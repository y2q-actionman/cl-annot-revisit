(in-package #:cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric operator-doc-type (operator)
    (:documentation "Returns the doc-type (in `cl:documentation') of OPERATOR.")
    (:method (operator)
      (case operator
        ((cl:defclass cl:define-condition cl:deftype)
         'cl:type)
        ((cl:defconstant cl:defparameter cl:defvar)
         'cl:variable)
        ((cl:defgeneric cl:defmacro cl:defun
           cl:define-modify-macro ; define-modify-macro == defmacro == `function'
           cl:lambda)
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
        ;; There is no docstring for `define-symbol-macro'. (cl-annot's comment is wrong.)
        (otherwise
         nil))))

  (defun expand-doc-macro-to-assign (docstring form doc-type)
    ;; Using the result of FORM (like below) is simple, but may
    ;; prevent compilation as a top-level form.
    (with-gensyms (obj)
      `(let ((,obj ,form))
         (setf (documentation ,obj ',doc-type) ,docstring)
         ,obj)))
  
  (defgeneric expand-documentation-using-head (operator docstring form)
    (:documentation "Called by `expand-documentation' to insert DOCSTRING into FORM.
If FORM can be expanded, returns its expansion. If not, returns FORM.")
    (:method (operator docstring form)
      (if-let ((doc-type (operator-doc-type operator)))
        (expand-doc-macro-to-assign docstring form doc-type)
        form))
    (:method ((operator (eql 'function)) docstring form)
      "Special handling for #'(lambda ..), adds DOCSTRING to the anonymous function."
      (if (starts-with 'lambda (second form))
          (expand-doc-macro-to-assign docstring form 'function)
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

(defmacro cl-annot-revisit:documentation (docstring &body body &environment env)
  "Insert DOCSTRING into FORMS."
  ;; Should I warn about 'setting same docstrings into many forms'?
  (apply-at-macro `(cl-annot-revisit:documentation ,docstring)
                  (alexandria:curry #'expand-documentation docstring) 
                  body env))

(defmacro cl-annot-revisit:doc (docstring &body body)
  "Just an alias of (cl-annot-revisit:documentation ...)"
  `(cl-annot-revisit:documentation ,docstring ,@body))
