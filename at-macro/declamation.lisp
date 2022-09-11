(in-package #:cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric declaration-target-operator-p (declaration-name operator)
    (:documentation "Returns true when DECLARATION-NAME is usable for OPERATOR.")
    (:method ((declaration-name (eql 'cl:special)) operator)
      (variable-definition-operator-p operator))
    (:method ((declaration-name (eql 'cl:type)) operator)
      (variable-definition-operator-p operator))
    (:method ((declaration-name (eql 'cl:ftype)) operator)
      (function-definition-operator-p operator))
    (:method ((declaration-name (eql 'cl:inline)) operator)
      (function-definition-operator-p operator))
    (:method ((declaration-name (eql 'cl:notinline)) operator)
      (function-definition-operator-p operator))
    (:method (declaration-name operator)
      (declare (ignore declaration-name operator))
      nil))

  (defun add-declaim-to-definiton-form (form decl-specifier)
    (let ((name (find-name-to-be-defined form)))
      `(progn (declaim (,@decl-specifier ,name))
              ,form)))

  (defgeneric expand-incompleted-declamation-using-head (operator decl-specifier form)
    (:documentation "Called by `expand-add-declamation' to expand FORM.")
    (:method (operator decl-specifier form)
      (let ((declaration-name (first decl-specifier)))
        (if (declaration-target-operator-p declaration-name operator)
            (add-declaim-to-definiton-form form decl-specifier)
            form))))

  (defun expand-incompleted-declamation (decl-specifier form)
    (macroexpand-convention (form)
      (if (consp form)
          (expand-incompleted-declamation-using-head (first form) decl-specifier form)
          form))))

(defmacro apply-incompleted-declamation (decl-specifier-head &body body &environment env)
  (apply-at-macro `(apply-incompleted-declamation ,decl-specifier-head)
                  (alexandria:curry #'expand-incompleted-declamation decl-specifier-head)
                  body env))

;;; Declaration and proclamation -- `type', `ftype', `inline', `notinline', `optimize', `special'

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun declaration-argument-like-p (first-form name-p-function env)
"Our macro for declamation is ambiguous when the first value is a list of names.
Consider:

  (cl-annot-revisit:inline
     (defun foo nil) ; defining a function takes zero arguments and returns nil.
     (defun bar () t))

I think it should expanded to next:

  (progn (declaim inline foo) (defun foo nil)
         (declaim inline bar) (defun bar () t))

Not like:

  (defun bar
    (declare (inline defun foo nil))
    t)

To distinguish a macro form from a list of names, I check the form is a macro-form or not."
    (or (null first-form)       ; '(cl-annot-revisit:inline () ...)
        (funcall name-p-function first-form) ; It is a name.
        (and (consp first-form)      ; It is like a list of names,
             (every name-p-function first-form)
             (not (special-operator-p (first first-form)))
             (not (macro-function (first first-form) env)))))

  (defun complete-declaration-specifier (declaration-head first-form name-p-function env)
    (if (declaration-argument-like-p first-form name-p-function env)
        (let ((names (ensure-list-with first-form name-p-function)))
          (values `(,@declaration-head ,@names) t)) ; completed
        (values declaration-head nil)))             ; not completed

  (defun expand-to-declaim-form (declaration-specifier)
    ;; This weird expansion is for use at top-level and in '#.' also.
    `(progn (declaim ,declaration-specifier)
            '(declare ,declaration-specifier)))

  (defun %expand-ambiguous-declamation (decl-head names-or-form body name-p-function env)
    (multiple-value-bind (decl-specifier completed?)
        (complete-declaration-specifier decl-head names-or-form name-p-function env)
      (cond
        ((not completed?)
         ;; Like '(cl-annot-revisit:inline (defun func nil) ...)'
         `(apply-incompleted-declamation ,decl-head
            ,names-or-form ,@body))
        ((not body)
         (expand-to-declaim-form decl-specifier))
        (t
         ;; Like '(cl-annot-revisit:notinline (x y z) ...)'
         `(add-declaration ,decl-specifier ,@body))))))

(defmacro cl-annot-revisit:special (&optional vars-or-form &body body &environment env)
  "Adds `special' declaration into BODY.
If BODY is nil, it is expanded to `declaim' and '(declare (special ...)), to embed it as a declaration using '#.'"
  (%expand-ambiguous-declamation '(cl:special) vars-or-form body
                                 #'symbolp env))

(defmacro cl-annot-revisit:type (typespec &optional vars-or-form &body body &environment env)
  (%expand-ambiguous-declamation `(cl:type ,typespec) vars-or-form body
                                 #'symbolp env))

(defmacro cl-annot-revisit:ftype (typespec &optional function-names-or-form &body body &environment env)
  (%expand-ambiguous-declamation `(cl:ftype ,typespec) function-names-or-form body
                                 #'function-name-p env))

(defmacro cl-annot-revisit:inline (&optional function-names-or-form &body body &environment env)
  (%expand-ambiguous-declamation '(cl:inline) function-names-or-form body
                                 #'function-name-p env))

(defmacro cl-annot-revisit:notinline (&optional function-names-or-form &body body &environment env)
  (%expand-ambiguous-declamation '(cl:notinline) function-names-or-form body
                                 #'function-name-p env))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +standard-optimize-quality-symbols+
      '(cl:compilation-speed cl:debug cl:safety cl:space cl:speed)
    :test 'equal)
  
  (defun common-lisp-package-p (package)
    (eql package (load-time-value (find-package '#:COMMON-LISP))))
  
  (defun optimize-quality-p (qual)
    ;; There may be implementation-dependent switch. I try to match loosely.
    (typecase qual
      (symbol
       (cond
         ((member qual +standard-optimize-quality-symbols+) t)
         ((common-lisp-package-p (symbol-package qual)) nil)
         (t t)))
      (cons
       (let ((1st (first qual)))
         (and (symbolp 1st)
              (cond
                ((member 1st +standard-optimize-quality-symbols+)
                 (and (length= 2 qual)
                      (typep (second qual) '(integer 0 3))))
                ((common-lisp-package-p (symbol-package 1st))
                 nil)
                (t t)))))
      (otherwise nil))))

(defmacro cl-annot-revisit:optimize (&optional qualities &body body)
  "Adds `optimize' declaration into BODY.
If BODY is nil, it is expanded to `declaim' and '(declare (optimize ...)), to embed it as a declaration using '#.'"
  (let ((decl-specifier `(cl:optimize ,@(ensure-list-with qualities #'optimize-quality-p))))
    (cond
      ((not body)
       (expand-to-declaim-form decl-specifier))
      (t
       `(add-declaration ,decl-specifier ,@body)))))

;;; Proclamation only -- `declaration'.

(defmacro cl-annot-revisit:declaration ((&rest names))
  "Just a shorthand of (declaim (declaration ...))"
  `(declaim (cl:declaration ,@names)))
