(in-package :cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *operator-doc-type-alist*
    '((defclass . type)
      (defconstant . variable)
      (defgeneric . function)
      (define-compiler-macro . compiler-macro)
      (define-condition . type)
      (define-method-combination . method-combination)
      (define-modify-macro . function) ; define-modify-macro == defmacro == `function'
      (define-setf-expander . setf)
      (defmacro . function)
      (defmethod . t)
      (defpackage . t)
      (defparameter . variable)
      (defsetf . setf)
      (defstruct . (structure type)) ; `defstruct' is specially treated.
      (deftype . type)
      (defun . function)
      (defvar . variable)
      ;; There is no docstring for `define-symbol-macro'.
      ))
  
  (defun operator-doc-type (name)
    (cdr (assoc name *operator-doc-type-alist*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric expand-@documentation-1* (operator form docstring)
    (:documentation "Called by `expand-@documentation-1' to insert DOCSTRING into FORM.
If FORM can be expanded, returns its expansion. If not, returns nil."))

  (defmethod expand-@documentation-1* (operator form docstring)
    "General case."
    (if-let ((doc-type (operator-doc-type operator)))
      ;; Using the result of FORM is simple, but may prevent compilation as a top-level form.
      (let ((obj (gensym)))
        `(let ((,obj ,form))
           (setf (documentation ,obj ',doc-type) ,docstring)
           ,obj))))

  ;; special handling for `defstruct' is in 'defstruct.lisp'.

  (defun warn-around-defclass (operator form)
    (when *at-macro-verbose*
      (warn 'at-macro-style-warning :form form
            :message (format nil "Adding documentation into ~A form does not works for slots."
                             operator))))
  
  (defmethod expand-@documentation-1* :before ((operator (eql 'defclass)) form docstring)
    (declare (ignore docstring))
    (warn-around-defclass operator form))

  (defmethod expand-@documentation-1* :before ((operator (eql 'define-condition)) form docstring)
    (declare (ignore docstring))
    (warn-around-defclass operator form))

  (defun warn-around-local-form (operator form)
    (when *at-macro-verbose*
      (warn 'at-macro-style-warning :form form
            :message (format nil "Adding declarations into ~A form doesn't work for local docstrings."
                             operator))))
  
  (defmethod expand-@documentation-1* :before ((operator (eql 'flet)) form docstring)
    (declare (ignore docstring))
    (warn-around-local-form operator form))

  (defmethod expand-@documentation-1* :before ((operator (eql 'labels)) form docstring)
    (declare (ignore docstring))
    (warn-around-local-form operator form))

  (defmethod expand-@documentation-1* :before ((operator (eql 'macrolet)) form docstring)
    (declare (ignore docstring))
    (warn-around-local-form operator form))

  
  (defmethod expand-@documentation-1* ((operator (eql 'lambda)) form docstring)
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
  
  (defmethod expand-@documentation-1* ((operator (eql 'function)) form docstring)
    "Special handling for #'(lambda ..), adds docstring to an anonymous function."
    (if (starts-with 'lambda (second form))
        `(function ,(expand-@documentation-1* 'lambda (second form) docstring))
        (call-next-method)))

  
  (defun expand-@documentation-1 (form docstring)
    "Insert DOCSTRING into FORM.
If insertion successed, returns (values <expansion> t).
If failed, returns (values <original-form> nil)."
    (try-macroexpand
     (if (consp form)
         (expand-@documentation-1* (first form) form docstring))
     form)))

(defmacro @documentation (docstring &body forms &environment env)
  ;; Should I warn about 'setting same docstrings into many forms'?
  (apply-at-macro `(@documentation ,docstring)
                  (lambda (form) (expand-@documentation-1 form docstring)) 
                  forms env))

(defmacro @doc (docstring form)
  "Just an alias of (@documentation ...)"
  `(@documentation ,docstring ,form))
