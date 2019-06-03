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


  (defgeneric insert-declaration-1* (operator form decl-specifier)
    (:documentation "Called by `insert-declaration-1' to insert DECL-SPECIFIER into FORM.
If FORM can be expanded, returns its expansion. If not, returns nil."))

  (defmethod insert-declaration-1* (operator form decl-specifier)
    "General case."
    (when (and (operator-take-local-declaration-p operator)
               *at-macro-verbose*)
      (warn 'at-declaration-style-warning :form form
            :message (format nil "Adding declarations into ~A form does not works for local declarations"
                             operator)))
    (if-let ((body-location (operator-body-location operator)))
      (insert-declaration-to-nth-body body-location form decl-specifier
                                      :documentation (operator-accept-docstring-in-body-p operator))))

  (defmethod insert-declaration-1* ((operator (eql 'defgeneric)) form decl-specifier)
    (unless (starts-with decl-specifier 'optimize)
      (error 'at-macro-error :form
            :message (format nil "`defgeneric' accepts only `optimize' declarations.")))
    (destructuring-bind (op function-name gf-lambda-list &rest option)
        form
      (when (and (assoc :method option)
                 *at-macro-verbose*)
        (warn 'at-declaration-style-warning :form form
              :message (format nil "Adding declarations into ~A form does not works for methods."
                               operator)))
      `(,op ,function-name ,gf-lambda-list (declare ,decl-specifier) ,@option)))

  (defmethod insert-declaration-1* ((operator (eql 'define-method-combination))
                                    form decl-specifier)
    (if (<= (length form) 3)
        (when *at-macro-verbose*
          (warn 'at-declaration-style-warning :form form
                :message "The short-form of `define-method-combination' doesn't take declarations."))
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

  (defmethod insert-declaration-1* ((operator (eql 'defmethod)) form decl-specifier)
    (destructuring-bind (op name &rest rest) form
      (let* ((method-qualifier (if (not (listp (first rest)))
                                   (pop rest)))
             (lambda-list (pop rest)))
        `(,op ,name ,@(if method-qualifier `(,method-qualifier)) ,lambda-list
              ,@(insert-declaration-to-body rest decl-specifier
                                            :whole form :documentation t)))))

  (defmethod insert-declaration-1* ((operator (eql 'defsetf)) form decl-specifier)
    (if (or (<= (length form) 3)
            (stringp (fourth form)))
        (when *at-macro-verbose*
          (warn 'at-declaration-style-warning
                :message "The short-form of `defsetf' does not take declarations."
                :form form))
        (insert-declaration-to-nth-body 4 form decl-specifier :documentation t)))


  (defun insert-declaration-1 (form decl-specifier)
    "Insert DECL-SPECIFIER into FORM.
If expansion successed, returns (values <expansion> t).
If failed, returns (values <original-form> nil)."
    (try-macroexpand
     (if (consp form)
         (insert-declaration-1* (first form) form decl-specifier))
     form)))


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
  (defun ignore-name-p (x)
    (typecase x
      (symbol t)
      (cons (starts-with 'function x))
      (otherwise nil)))

  (defun expand-at-declaration (declaration-name names body)
    (let* ((names (ensure-list-with names #'ignore-name-p))
           (decl-specifier `(,declaration-name ,@names)))
      (if body
          `(@add-declaration ,decl-specifier ,@body)
          `'(declare ,decl-specifier)))))

(defmacro @ignore (names &body body)
  "Adds `ignore' declaration into BODY.
If BODY is nil, it is expanded to '(declare (ignore ...)), this is intended to embed it as a declaration using '#.'"
  (expand-at-declaration 'ignore names body))

(defmacro @ignorable (names &body body)
  "Adds `ignorable' declaration into BODY.
If BODY is nil, it is expanded to '(declare (ignorable ...)), this is intended to embed it as a declaration using '#.'"
  (expand-at-declaration 'ignorable names body))

(defmacro @dynamic-extent (names &body body)
  "Adds `dynamic-extent' declaration into BODY.
If BODY is nil, it is expanded to '(declare (dynamic-extent ...)), this is intended to embed it as a declaration using '#.'"
  (expand-at-declaration 'dynamic-extent names body))
