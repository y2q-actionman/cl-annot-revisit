(in-package #:cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric operator-body-location (symbol)
    (:documentation "When SYMBOL naming an operator which can be
    treated by our at-macros for declaration, returns an integer where
    its body locates.")
    (:method (_)
      (declare (ignore _))
      nil)
    (:method ((symbol symbol))
      (case symbol
        ((cl:locally)
         1)
        ((cl:do-all-symbols cl:do-external-symbols cl:do-symbols cl:dolist
           cl:dotimes cl:flet cl:labels cl:lambda cl:let cl:let* cl:macrolet
           cl:pprint-logical-block cl:prog cl:prog* cl:symbol-macrolet
           cl:with-hash-table-iterator cl:with-input-from-string
           cl:with-open-file cl:with-open-stream cl:with-output-to-string
           cl:with-package-iterator)
         2)
        ((cl:define-compiler-macro cl:define-setf-expander cl:defmacro
           cl:deftype cl:defun cl:destructuring-bind cl:do cl:do*
           cl:multiple-value-bind cl:with-accessors cl:with-slots)
         3)
        (otherwise
         nil))))

  (define-constant +standard-operators-accept-docstring-in-body+
      '(cl:define-compiler-macro cl:define-setf-expander cl:defmacro
        cl:deftype cl:defun cl:lambda)
    :test 'equal)

  (defgeneric operator-accept-docstring-in-body-p (symbol)
    (:documentation "Returns T if SYMBOL naming an operator accepts docstring in its body.")
    (:method (_)
      (declare (ignore _))
      nil)
    (:method ((symbol symbol))
      (member symbol +standard-operators-accept-docstring-in-body+)))

  (define-constant +standard-operators-take-local-declaration+
      '(cl:flet cl:labels cl:macrolet cl:handler-case cl:restart-case)
    :test 'equal)

  (defgeneric operator-take-local-declaration-p (symbol)
    (:documentation "Returns T if SYMBOL naming an operator may take local declarations.")
    (:method (_)
      (declare (ignore _))
      nil)
    (:method ((symbol symbol))
      (member symbol +standard-operators-take-local-declaration+)))

  
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


  (defgeneric expand-add-declaration-using-head (operator decl-specifier form)
    (:documentation "Called by `expand-add-declaration' to insert DECL-SPECIFIER into FORM.
If FORM can be expanded, returns the expansion. If not, returns FORM.")
    (:method (operator decl-specifier form)
      "General case."
      (when (and (operator-take-local-declaration-p operator)
                 *at-macro-verbose*)
        (warn 'at-macro-style-warning :form form
              :message (format nil "Adding declarations into ~A form does not works for local declarations."
                               operator)))
      (if-let ((body-location (operator-body-location operator)))
        (insert-declaration-to-nth-body body-location form decl-specifier
                                        :documentation (operator-accept-docstring-in-body-p operator))
        form))
    (:method ((operator (eql 'defgeneric)) decl-specifier form)
      (unless (starts-with decl-specifier 'optimize)
        (error 'at-macro-error :form
               :message (format nil "`defgeneric' accepts only `optimize' declarations.")))
      (destructuring-bind (op function-name gf-lambda-list &rest option)
          form
        (when (and (assoc :method option)
                   *at-macro-verbose*)
          (warn 'at-macro-style-warning :form form
                :message (format nil "Adding declarations into ~A form does not works for methods."
                                 operator)))
        `(,op ,function-name ,gf-lambda-list (declare ,decl-specifier) ,@option)))
    (:method ((operator (eql 'define-method-combination)) decl-specifier form)
      (cond
        ((<= (length form) 3)
         (when *at-macro-verbose*
           (warn 'at-macro-style-warning
                 :message "The short-form of `define-method-combination' doesn't take declarations."
                 :form form))
         form)
        (t
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
                                                 :whole form :documentation t)))))))
    (:method ((operator (eql 'defmethod)) decl-specifier form)
      (destructuring-bind (op name &rest rest) form
        (let* ((method-qualifier (if (not (listp (first rest)))
                                     (pop rest)))
               (lambda-list (pop rest)))
          `(,op ,name ,@(if method-qualifier `(,method-qualifier)) ,lambda-list
                ,@(insert-declaration-to-body rest decl-specifier
                                              :whole form :documentation t)))))
    (:method ((operator (eql 'defsetf)) decl-specifier form)
      (cond
        ((or (<= (length form) 3)
             (stringp (fourth form)))
         (when *at-macro-verbose*
           (warn 'at-macro-style-warning
                 :message "The short-form of `defsetf' does not take declarations."
                 :form form))
         form)
        (t
         (insert-declaration-to-nth-body 4 form decl-specifier :documentation t)))))

  (defun expand-add-declaration (decl-specifier form)
    "Insert DECL-SPECIFIER into FORM.
If expansion successed, returns (values <expansion> t).
If failed, returns (values <original-form> nil)."
    (macroexpand-convention (form)
      (if (consp form)
          (expand-add-declaration-using-head (first form) decl-specifier form)
          form))))


(defmacro add-declaration (decl-specifier &body body &environment env)
  "Used by at-macros of declarations for processing recursive expansion.
If BODY is a form accepts declarations, adds a DECL-SPECIFIER into it.
If not, wraps BODY with `locally' containing DECL-SPECIFIER in it."
  (apply-at-macro `(add-declaration ,decl-specifier)
                  (lambda (form) (expand-add-declaration decl-specifier form))
                  body env
                  :if-no-expansion
                  (lambda (form) `(locally (declare ,decl-specifier)
                                    ,form))))

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
          `(add-declaration ,decl-specifier ,@body)
          `'(declare ,decl-specifier)))))

(defmacro cl-annot-revisit:ignore (names &body body)
  "Adds `cl:ignore' declaration into BODY.
If BODY is nil, it is expanded to '(declare (ignore ...)), to embed it as a declaration using '#.'"
  (expand-at-declaration 'cl:ignore names body))

(defmacro cl-annot-revisit:ignorable (names &body body)
  "Adds `ignorable' declaration into BODY.
If BODY is nil, it is expanded to '(declare (ignorable ...)), to embed it as a declaration using '#.'"
  (expand-at-declaration 'cl:ignorable names body))

(defmacro cl-annot-revisit:dynamic-extent (names &body body)
  "Adds `dynamic-extent' declaration into BODY.
If BODY is nil, it is expanded to '(declare (dynamic-extent ...)), to embed it as a declaration using '#.'"
  (expand-at-declaration 'cl:dynamic-extent names body))
