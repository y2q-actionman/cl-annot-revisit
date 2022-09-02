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

  (defgeneric expand-add-declamation-using-head (operator decl-specifier form)
    (:documentation "Called by `expand-add-declamation' to expand FORM.")
    (:method (operator decl-specifier form)
      (let ((declaration-name (first decl-specifier)))
        (if (declaration-target-operator-p declaration-name operator)
            (add-declaim-to-definiton-form form decl-specifier)
            form))))

  ;; Supporting `declaim' and `proclaim' is easy, but are they meaningful?
  ;;   (cl-annot-revisit:inline (func-a) (declaim)) ; => (declaim (inline func-a))
  
  (defun expand-add-declamation (decl-specifier form)
    (macroexpand-convention (form)
      (if (consp form)
          (expand-add-declamation-using-head (first form) decl-specifier form)
          form))))

(defmacro add-declamation (decl-specifier &body body &environment env)
  (apply-at-macro `(add-declamation ,decl-specifier)
                  (alexandria:curry #'expand-add-declamation decl-specifier)
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
  
  (defun expand-at-declamation (decl-head names-or-form body name-p-function env)
    (if (declaration-argument-like-p names-or-form name-p-function env)
        ;; Treats as declaration.
        ;; Like '(cl-annot-revisit:notinline (x y z) ...)'
        (let* ((names (ensure-list-with names-or-form name-p-function))
               (decl-specifier `(,@decl-head ,@names)))
          (if body
              `(add-declaration ,decl-specifier ,@body) ; Use it as a local declaration.
              ;; This weird expansion is for use at top-level and in '#.' also.
              `(progn (declaim ,decl-specifier)
                      '(declare ,decl-specifier))))
        ;; Treats as declamation.
        ;; Like '(cl-annot-revisit:inline (defun func nil) ...)'
        `(add-declamation ,decl-head
           ,names-or-form ,@body))))

(defmacro cl-annot-revisit:special (&optional vars-or-form &body body &environment env)
  "Adds `special' declaration into BODY.
If BODY is nil, it is expanded to `declaim' and '(declare (special ...)), to embed it as a declaration using '#.'"
  (expand-at-declamation '(cl:special) vars-or-form body
                         #'symbolp env))

(defmacro cl-annot-revisit:type (typespec &optional vars-or-form &body body &environment env)
  (expand-at-declamation `(cl:type ,typespec) vars-or-form body
                         #'symbolp env))

(defmacro cl-annot-revisit:ftype (typespec &optional function-names-or-form &body body &environment env)
  (expand-at-declamation `(cl:ftype ,typespec) function-names-or-form body
                         #'function-name-p env))

(defmacro cl-annot-revisit:inline (&optional function-names-or-form &body body &environment env)
  (expand-at-declamation '(cl:inline) function-names-or-form body
                         #'function-name-p env))

(defmacro cl-annot-revisit:notinline (&optional function-names-or-form &body body &environment env)
  (expand-at-declamation '(cl:notinline) function-names-or-form body
                         #'function-name-p env))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun optimize-quality-p (x)
    (typecase x
      (symbol t)
      ;; There may be implementation-dependent switch. I try to match loosely.
      (cons (and (symbolp (first x))
                 (every #'atom (rest x)))) ; seeing '(speed 3)' etc.
      (otherwise nil))))

(defmacro cl-annot-revisit:optimize (qualities &body body &environment env)
  "Adds `optimize' declaration into BODY.
If BODY is nil, it is expanded to `declaim' and '(declare (optimize ...)), to embed it as a declaration using '#.'"
  (expand-at-declamation `(cl:optimize ,@(ensure-list-with qualities #'optimize-quality-p))
                         nil body
                         (constantly nil) env))


;;; Proclamation only -- `declaration'.

(defmacro cl-annot-revisit:declaration ((&rest names))
  "Just a shorthand of (declaim (declaration ...))"
  `(declaim (cl:declaration ,@names)))
