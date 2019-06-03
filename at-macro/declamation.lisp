(in-package :cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric insert-declamation-1* (declaration form decl-specifier)
    (:method (declaration form decl-specifier)
      (declare (ignore operator declaration form decl-specifier))
      nil))

  (defun add-declaim-to-definiton-form (form decl-specifier)
    (let ((name (find-name-to-be-defined form)))
      `(progn (declaim (,@decl-specifier ,name))
              ,form)))

  (defmethod insert-declamation-1* ((declaration (eql 'special)) form decl-specifier)
    (if (variable-definition-operator-p (first form)) ; FORM is like `defvar'.
        (add-declaim-to-definiton-form form decl-specifier)))

  (defmethod insert-declamation-1* ((declaration (eql 'type)) form decl-specifier)
    (if (variable-definition-operator-p (first form)) ; FORM is like `defvar'.
        (add-declaim-to-definiton-form form decl-specifier)))

  (defmethod insert-declamation-1* ((declaration (eql 'ftype)) form decl-specifier)
    (if (function-definition-operator-p (first form)) ; FORM is like `defun'.
        (add-declaim-to-definiton-form form decl-specifier)))

  (defmethod insert-declamation-1* ((declaration (eql 'inline)) form decl-specifier)
    (if (function-definition-operator-p (first form))
        (add-declaim-to-definiton-form form decl-specifier)))

  (defmethod insert-declamation-1* ((declaration (eql 'notinline)) form decl-specifier)
    (if (function-definition-operator-p (first form))
        (add-declaim-to-definiton-form form decl-specifier)))

  (defun insert-declamation-1 (form decl-specifier)
    (try-macroexpand
     (if (consp form)
         (insert-declamation-1* (first decl-specifier) form decl-specifier))
     form)))


(defmacro @add-declamation (decl-specifier &body body &environment env)
  (let ((at-macro-form `(@add-declamation ,decl-specifier)))
    (cond
      ((not body)
       nil)
      ((not (length= 1 body))           ; recursive expansion
       `(progn ,@(apply-to-all-forms at-macro-form body)))
      (t
       (let ((form (first body)))
         (mv-cond-let2 (expansion expanded-p)
           ((insert-declamation-1 form decl-specifier)) ; try known expansions.
           ((apply-to-special-form-1 at-macro-form form)) ; try recursive expansion.
           ((macroexpand-1 form env) ; try `macroexpand-1'.
            `(,@at-macro-form ,expansion))
           (t
            form)))))))

;;; Declaration and proclamation -- `type', `ftype', `inline', `notinline', `optimize', `special'

#|
This macro is ambiguous when the first value is a list of names.
Here is an example:

  (@inline
     (defun foo nil) ; defining a function takes zero arguments and returns nil.
     (defun bar () t))


I think it should expanded to below:

  (progn (declaim inline foo) (defun foo nil)
         (declaim inline bar) (defun bar () t))

Not like:

  (defun bar
    (declare (inline defun foo nil))
    t)


To distinguish a macro form from a list of names, I try to `macroexpand-1' to the form.
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-at-declamation (decl-head names-or-form body name-p-function env)
    (cond
      ((and (not (funcall name-p-function names-or-form))
            (consp names-or-form)
            (every name-p-function names-or-form)
            (nth-value 1 (macroexpand-1 names-or-form env))) ; TODO: use env.
       ;; Like '(@inline (defun func nil) ...)'
       `(@add-declamation ,decl-head
                          ;; I don't use `macroexpand-1' result,
                          ;; because other at-macros may want to see original forms.
                          ,names-or-form ,@body))
      (t                             ; Like '(@notinline (x y z) ...)'
       (let* ((names (ensure-list-with names-or-form name-p-function))
              (decl-specifier `(,@decl-head ,@names)))
         (if body
             `(@add-declaration ,decl-specifier ,@body) ; Use it as a local declaration.
             `(progn (declaim ,decl-specifier)
                     '(declare ,decl-specifier))))))))

(defmacro @special (&optional vars-or-form &body body &environment env)
  ;; TODO: add 'VARS-OR-FORM' treating
  "Adds `special' declaration into BODY.
If BODY is nil, it is expanded to `declaim' and '(declare (special ...)), this is intended to embed it as a declaration using '#.'"
  (expand-at-declamation '(special) vars-or-form body
                         #'symbolp env))

(defmacro @type (typespec &optional vars-or-form &body body &environment env)
  (expand-at-declamation `(type ,typespec) vars-or-form body
                         #'symbolp env))
;;; FIXME: what is relationships between '@type' and `the'?

(defmacro @ftype (typespec &optional function-names-or-form &body body &environment env)
  (expand-at-declamation `(ftype ,typespec) function-names-or-form body
                         #'function-name-p env))

(defmacro @inline (&optional function-names-or-form &body body &environment env)
  (expand-at-declamation '(inline) function-names-or-form body
                         #'function-name-p env))

(defmacro @notinline (&optional function-names-or-form &body body &environment env)
  (expand-at-declamation '(notinline) function-names-or-form body
                         #'function-name-p env))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun optimize-quality-p (x)
    (typecase x
      (symbol t)
      ;; There may be implementation-dependent switch. I try to match loosely.
      (cons (and (symbolp (first x))
                 (every #'atom (rest x)))) ; seeing '(speed 3)' etc.
      (otherwise nil))))

(defmacro @optimize (qualities &body body &environment env)
  "Adds `optimize' declaration into BODY.
If BODY is nil, it is expanded to `declaim' and '(declare (optimize ...)), this is intended to embed it as a declaration using '#.'"
  (expand-at-declamation `(optimize ,@(ensure-list-with qualities #'optimize-quality-p))
                         nil body
                         (constantly nil) env))

;;; Supporting `declaim' and `proclaim' is easy, but are they meaningful?
;;;   (@inline (func-a) (declaim)) ; => (declaim (inline func-a))


;;; Proclamation only -- `declaration'.

(defmacro @declaration ((&rest names))
  "Just a shorthand of (declaim (declaration ...))"
  `(declaim (declaration ,@names)))
