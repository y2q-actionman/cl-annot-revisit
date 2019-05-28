(in-package :cl-annot-revisit/at-macro)

(define-condition at-declaration-style-warning (at-macro-style-warning)
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *operator-body-location-alist*
    (append '((locally . 1))
            (mapcar
             (lambda (op) (cons op 2))
             '(do-all-symbols do-external-symbols do-symbols dolist
               dotimes flet labals lambda let let* macrolet
               pprint-logical-block prog prog* symbol-macrolet
               with-hash-table-iterator with-input-from-string
               with-open-file with-open-stream with-output-to-string
               with-package-iterator))
            (mapcar
             (lambda (op) (cons op 3))
             '(define-compiler-macro define-setf-expander defmacro
               deftype defun destructuring-bind do do*
               multiple-value-bind with-accessors with-slots)))
    "Alist of operators which can be treated by our at-macros for declaration.")
  
  (defun operator-body-location (name)
    (cdr (assoc name *operator-body-location-alist*)))

  (defparameter *operators-accept-docstring-in-body*
    '(define-compiler-macro define-setf-expander defmacro deftype
      defun lambda)
    "List of operators accepts docstring in its body.")

  (defun operator-accept-docstring-in-body-p (name)
    (member name *operators-accept-docstring-in-body*))

  (defparameter *operators-take-local-declaration*
    '(flet labels macrolet handler-case restart-case)
    "List of operators may take local declarations.")

  (defun operator-take-local-declaration-p (name)
    (member name *operators-take-local-declaration*))

  
  (defun insert-declaration-to-body (form-body decl-specifier &key documentation whole)
    (multiple-value-bind (body decls doc)
        (parse-body form-body :documentation documentation :whole whole)
      `((declare ,decl-specifier)
        ,@decls
        ,@(if doc `(,doc)) 
        ,@body)))

  (defun insert-declaration-to-nth-body (body-index form decl-specifier &key documentation)
    (multiple-value-bind (head body)
        (split-list-at body-index form)
      `(,@head
        ,@(insert-declaration-to-body body decl-specifier
                                      :documentation documentation :whole form))))


  (defgeneric insert-declaration-1* (operator declaration form decl-specifier)
    (:documentation "Called by `insert-declaration-1' to insert DECL-SPECIFIER into FORM.
If FORM can be expanded, returns its expansion. If not, returns nil."))

  (defmethod insert-declaration-1* (operator declaration form decl-specifier)
    "General case."
    (declare (ignore declaration))
    (when (operator-take-local-declaration-p operator)
      (warn 'at-declaration-style-warning :form form
            :message (format nil "Adding declarations into ~A form does not works for local declarations"
                             operator)))
    (if-let ((body-location (operator-body-location operator)))
      (insert-declaration-to-nth-body body-location form decl-specifier
                                      :documentation (operator-accept-docstring-in-body-p operator))))

  (defmethod insert-declaration-1* ((operator (eql 'defgeneric)) declaration
                                    form decl-specifier)
    (unless (starts-with declaration 'optimize)
      (warn 'at-declaration-style-warning :form
            :message (format nil "`defgeneric' accepts only `optimize' declarations.")))
    (destructuring-bind (op function-name gf-lambda-list &rest option)
        form
      (when (assoc :method option)
        (warn 'at-declaration-style-warning :form form
              :message (format nil "Adding declarations into ~A form does not works for methods."
                               operator)))
      `(,op ,function-name ,gf-lambda-list (declare ,decl-specifier) ,@option)))

  (defmethod insert-declaration-1* ((operator (eql 'define-method-combination)) declaration form decl-specifier)
    (declare (ignore declaration))
    (if (<= (length form) 3)
        (warn 'at-declaration-style-warning :form form
              :message "The short-form of `define-method-combination' doesn't take declarations.")
        (destructuring-bind (op name lambda-list (&rest method-group-specifier) &rest rest)
            form
          (let (options)
            (when (starts-with :arguments (first rest))
              (push (pop rest) options))
            (when (starts-with :generic-function (first rest))
              (push (pop rest) options))
            (nreversef options)
            `(,op ,name ,lambda-list (,@method-group-specifier)
                  ,@options
                  ,@(insert-declaration-to-body rest decl-specifier
                                                :whole form :documentation t))))))

  (defmethod insert-declaration-1* ((operator (eql 'defmethod)) declaration form decl-specifier)
    (declare (ignore declaration))
    (destructuring-bind (op name &rest rest) form
      (let* ((method-qualifier (if (not (listp (first rest)))
                                   (pop rest)))
             (lambda-list (pop rest)))
        `(,op ,name ,@(if method-qualifier `(,method-qualifier)) ,lambda-list
              ,@(insert-declaration-to-body rest decl-specifier
                                            :whole form :documentation t)))))

  (defmethod insert-declaration-1* ((operator (eql 'defsetf)) declaration form decl-specifier)
    (declare (ignore declaration))
    (if (or (<= (length form) 3)
            (stringp (fourth form)))
        (warn 'at-declaration-style-warning
              :message "The short-form of `defsetf' does not take declarations."
              :form form)
        (insert-declaration-to-nth-body 4 form decl-specifier :documentation t)))

  (defun try-add-declaim-to-definiton-form (form decl-specifier decl-args-index)
    (let ((name (find-name-to-be-defined form)))
      (multiple-value-bind (decl-head decl-args) (split-list-at decl-args-index decl-specifier)
        (if (member name decl-args)     
            ;; If decl-specifier contains for this variable, apply it using `declaim',
            ;; and apply other declarations recursively.
            (let ((rest-decl-args (remove name decl-args)))
              `(progn (declaim (,@decl-head ,name))
                      ,(if rest-decl-args
                           (insert-declaration-1 form `(,@decl-head ,@rest-decl-args))
                           form)))
            nil))))

  (defmethod insert-declaration-1* (operator (declaration (eql 'special)) form decl-specifier)
    (or (and (variable-definition-operator-p operator) ; FORM is like `defvar'.
             (try-add-declaim-to-definiton-form form decl-specifier 1))
        (call-next-method)))

  (defmethod insert-declaration-1* (operator (declaration (eql 'type)) form decl-specifier)
    (or (and (variable-definition-operator-p operator) ; FORM is like `defvar'.
             (try-add-declaim-to-definiton-form form decl-specifier 2))
        (call-next-method)))

  (defmethod insert-declaration-1* (operator (declaration (eql 'ftype)) form decl-specifier)
    (or (and (function-definition-operator-p operator) ; FORM is like `defun'.
             (try-add-declaim-to-definiton-form form decl-specifier 2))
        (call-next-method)))

  (defmethod insert-declaration-1* (operator (declaration (eql 'inline)) form decl-specifier)
    (or (and (function-definition-operator-p operator)
             (try-add-declaim-to-definiton-form form decl-specifier 1))
        (call-next-method)))

  (defmethod insert-declaration-1* (operator (declaration (eql 'notinline)) form decl-specifier)
    (or (and (function-definition-operator-p operator)
             (try-add-declaim-to-definiton-form form decl-specifier 1))
        (call-next-method)))

  ;; Supporting `declaim' and `proclaim' is easy, but are they meaningful?
  ;;   (@inline (func-a) (declaim)) ; => (declaim (inline func-a))


  (defun insert-declaration-1 (form decl-specifier)
    "Insert DECL-SPECIFIER into FORM.
If expansion successed, returns (values <expansion> t).
If failed, returns (values <original-form> nil)."
    (typecase form
      (cons
       (if-let ((expansion (insert-declaration-1* (first form) (first decl-specifier)
                                                  form decl-specifier)))
         (values expansion t)
         (values form nil)))
      (otherwise (values form nil))))) 


(defmacro @add-declaration (decl-specifier &body body &environment env)
  "Used by at-macros of declarations for processing recursive expansion.
If BODY is a form accepts declarations, adds a DECL-SPECIFIER into it.
If not, wraps BODY with `locally' containing DECL-SPECIFIER in it."
  (let ((at-macro-form `(@add-declaration ,decl-specifier)))
    (cond
      ((not body)
       nil)
      ((not (length= 1 body))           ; recursive expansion
       `(progn ,@(apply-to-all-forms at-macro-form body)))
      (t
       (let ((form (first body)))
         (mv-cond-let2 (expansion expanded-p)
           ((insert-declaration-1 form decl-specifier)) ; try known expansions.
           ((apply-to-special-form-1 at-macro-form form)) ; try recursive expansion.
           ((macroexpand-1 form env) ; try `macroexpand-1'.
            `(,@at-macro-form ,expansion))
           (t
            `(locally (declare ,decl-specifier)
               ,form))))))))

;;; Declaration only -- `ignore', `ignorable', `dynamic-extent'

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun local-declaration-name-p (x)
    (typecase x
      (symbol t)
      (cons (starts-with 'function x))
      (otherwise nil)))
  
  (defun expand-local-declaration (decl-name names body)
    (let* ((names-list (ensure-list-with names #'local-declaration-name-p))
           (new-decl `(,decl-name ,@names-list)))
      (if body
          `(@add-declaration ,new-decl ,@body)
          `'(declare ,new-decl)))))

(defmacro @ignore (names &body body)
  "Adds `ignore' declaration into BODY.
If BODY is nil, it is expanded to '(declare (ignore ...)), this is intended to embed it as a declaration using '#.'"
  (expand-local-declaration 'ignore names body))

(defmacro @ignorable (names &body body)
  "Adds `ignorable' declaration into BODY.
If BODY is nil, it is expanded to '(declare (ignorable ...)), this is intended to embed it as a declaration using '#.'"
  (expand-local-declaration 'ignorable names body))

(defmacro @dynamic-extent (names &body body)
  "Adds `dynamic-extent' declaration into BODY.
If BODY is nil, it is expanded to '(declare (dynamic-extent ...)), this is intended to embed it as a declaration using '#.'"
  (expand-local-declaration 'dynamic-extent names body))

;;; Declaration and proclamation -- `type', `ftype', `inline', `notinline', `optimize', `special'

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun optimize-quality-p (x)
    (typecase x
      (symbol t)
      ;; There may be implementation-dependent switch. I try to match loosely.
      (cons (and (symbolp (first x))
                 (every #'atom (rest x)))) ; seeing '(speed 3)' etc.
      (otherwise nil)))
  
  (defun expand-declaration-and-proclamation (new-decl body)
    "If BODY is a form accepts declarations, adds a declaration NEW-DECL into it.
If BODY is nil, it is expanded to `declaim' and '(declare NEW-DECL), this is intended to embed it as a declaration using '#.'"
    (if body
        `(@add-declaration ,new-decl ,@body)
        `(progn (declaim ,new-decl)
                '(declare ,new-decl)))))

(defmacro @optimize (qualities &body body)
  "Adds `optimize' declaration into BODY.
If BODY is nil, it is expanded to `declaim' and '(declare (optimize ...)), this is intended to embed it as a declaration using '#.'"
  (let ((qualities-list (ensure-list-with qualities #'optimize-quality-p)))
    (expand-declaration-and-proclamation `(optimize ,@qualities-list) body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; TODO: I should see all forms whether it is definition form or not.
  (defun expand-at-declaration-may-definition-at-first
      (decl-head names-or-form body operator-p-function name-p-function)
    (cond
      ((null names-or-form)         ; Like '(@special () ...)'
       `(progn ,@body))             ; It is simply treated as `progn'.
      ((and (consp names-or-form)
            (funcall operator-p-function (first names-or-form)))
       ;; Like '(@inline (defun func () ...))'
       ;; It is treated as '(@inline (func) (defun func () ...)'.
       (let ((var (find-name-to-be-defined names-or-form))
             (body (list* names-or-form body)))
         (expand-declaration-and-proclamation `(,@decl-head ,var) body)))
      (t                             ; Like '(@notinline (x y z) ...)'
       (let ((names (ensure-list-with names-or-form name-p-function)))
         (expand-declaration-and-proclamation `(,@decl-head ,@names) body)))))

  (defun expand-at-declaration-for-variable (decl-head vars-or-form body)
    (expand-at-declaration-may-definition-at-first
     decl-head vars-or-form body
     #'variable-definition-operator-p
     #'symbolp))

  (defun expand-at-declaration-for-function (decl-head fnames-or-form body)
    (expand-at-declaration-may-definition-at-first
     decl-head fnames-or-form body
     #'function-definition-operator-p
     #'function-name-p)))

(defmacro @special (&optional vars-or-form &body body)
  ;; TODO: add 'VARS-OR-FORM' treating
  "Adds `special' declaration into BODY.
If BODY is nil, it is expanded to `declaim' and '(declare (special ...)), this is intended to embed it as a declaration using '#.'"
  (expand-at-declaration-for-variable '(special) vars-or-form body))

(defmacro @type (typespec &optional vars-or-form &body body)
  ;; TODO: add docstrings
  (expand-at-declaration-for-variable `(type ,typespec) vars-or-form body))

(defmacro @ftype (typespec &optional function-names-or-form &body body)
  ;; TODO: add docstrings
  (expand-at-declaration-for-function `(ftype ,typespec) function-names-or-form body))

(defmacro @inline (&optional function-names-or-form &body body)
  ;; TODO: add docstrings
  (expand-at-declaration-for-function '(inline) function-names-or-form body))

(defmacro @notinline (&optional function-names-or-form &body body)
  ;; TODO: add docstrings
  (expand-at-declaration-for-function '(notinline) function-names-or-form body))

;;; Proclamation only -- `declaration'.

(defmacro @declaration ((&rest names))
  "Just a shorthand of (declaim (declaration ...))"
  `(declaim (declaration ,@names)))


;;; FIXME: what is '@type' and `the'?
