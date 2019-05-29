(in-package :cl-annot-revisit/at-macro)

(define-condition @documentation-style-warning (at-macro-style-warning)
  ())

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
  (defgeneric insert-documentation-1* (operator form docstring)
    (:documentation "Called by `insert-documentation-1' to insert DOCSTRING into FORM.
If FORM can be expanded, returns its expansion. If not, returns nil."))

  (defmethod insert-documentation-1* (operator form docstring)
    "General case."
    (if-let ((doc-type (operator-doc-type operator)))
      ;; Using the result of FORM is simple, but may prevent compilation as a top-level form.
      (let ((obj (gensym)))
        `(let ((,obj ,form))
           (setf (documentation ,obj ',doc-type) ,docstring)
           ,obj))))

  (defmethod insert-documentation-1* ((operator (eql 'defstruct)) form docstring)
    "Special handling for `defstruct'"
    (let* ((name-or-options (second form))
           (type-supplied-p
            (etypecase name-or-options
              (symbol nil)
              (list (find-if (lambda (x) (starts-with :type x)) name-or-options)))))
      `(let ((name ,form))
         (setf (documentation name 'structure) ,docstring)
         ,@(if type-supplied-p
               `((setf (documentation name 'type) ,docstring))))))

  (defun warning-around-defclass (form)
    (warn '@documentation-style-warning :form form
          :message (format nil "Adding declarations into ~A form does not works for slots."
                           (first form))))
  
  (defmethod insert-documentation-1* :before ((operator (eql 'defclass)) form docstring)
    (declare (ignore docstring))
    (warning-around-defclass form))

  (defmethod insert-documentation-1* :before ((operator (eql 'define-condition)) form docstring)
    (declare (ignore docstring))
    (warning-around-defclass form))

  (defun warning-around-local-form (form)
    (warn '@documentation-style-warning :form form
          :message (format nil "Adding declarations into ~A form doesn't work for local docstrings."
                           (first form))))
  
  (defmethod insert-documentation-1* :before ((operator (eql 'flet)) form docstring)
    (declare (ignore docstring))
    (warning-around-local-form form))

  (defmethod insert-documentation-1* :before ((operator (eql 'labels)) form docstring)
    (declare (ignore docstring))
    (warning-around-local-form form))

  (defmethod insert-documentation-1* :before ((operator (eql 'macrolet)) form docstring)
    (declare (ignore docstring))
    (warning-around-local-form form))

  
  (defmethod insert-documentation-1* ((operator (eql 'lambda)) form docstring)
    "Special handling for `lambda', adds docstring to an anonymous function."
    (destructuring-bind (op lambda-list body0) form
      (multiple-value-bind (body decls old-doc)
          (parse-body body0 :documentation t :whole form)
        (when (and old-doc docstring)
          (warn '@documentation-style-warning :form form
                :message (format nil "Lambda form already has a docstring")))
        `(,op ,lambda-list ,decls
              ,@(cond (docstring (list docstring))
                      (old-doc (list old-doc))
                      (t nil))
              ,@body))))
  
  (defmethod insert-documentation-1* ((operator (eql 'function)) form docstring)
    "Special handling for (function (lambda ..)), adds docstring to an anonymous function."
    (if (starts-with 'lambda (second form))
        `(function ,(insert-documentation-1* 'lambda (second form) docstring))
        (call-next-method)))
  

  (defun insert-documentation-1 (form docstring)
    "Insert DOCSTRING into FORM.
If insertion successed, returns (values <expansion> t).
If failed, returns (values <original-form> nil)."
    (typecase form
      (cons
       (if-let ((expansion (insert-documentation-1* (first form) form docstring)))
         (values expansion t)
         (values form nil)))
      (otherwise (values form nil)))))

(defmacro @documentation (docstring form &environment env)
  (mv-cond-let2 (expansion expanded-p)
    ((insert-documentation-1 form docstring))
    ;; Should I warn about 'setting same docstrings into many forms'?
    ((apply-to-special-form-1 `(@documentation docstring) form))
    ((macroexpand-1 form env)
     `(@documentation ,docstring ,expansion))
    (t form)))

(defmacro @doc (docstring form)
  "Just an alias of (@documentation ...)"
  `(@documentation ,docstring ,form))
