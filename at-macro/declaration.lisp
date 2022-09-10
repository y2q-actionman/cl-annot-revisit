(in-package #:cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric operator-body-location (operator)
    (:documentation "When OPERATOR can be treated by our at-macros for
    declaration, returns an integer where its body locates.")
    (:method (_)
      (declare (ignore _))
      nil)
    (:method ((operator symbol))
      (case operator
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
        ((cl:defsetf cl:define-method-combination cl:defmethod)
         ;; These may or may not takes body and its location is variable.
         t)
        (otherwise nil))))

  (define-constant +standard-operators-accept-docstring-in-body+
      '(cl:define-compiler-macro cl:define-setf-expander cl:defmacro
        cl:defmethod cl:deftype cl:defun cl:lambda
        cl:define-method-combination cl:defsetf) ; only in long-form.
    :test 'equal)

  (defgeneric operator-accept-docstring-in-body-p (operator)
    (:documentation "Returns T if OPERATOR accepts docstring in its body.")
    (:method (_)
      (declare (ignore _))
      nil)
    (:method ((operator symbol))
      (member operator +standard-operators-accept-docstring-in-body+)))

  
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

  (defun parse-define-method-combination-long-form (define-method-combination-form)
    "Destructs `define-method-combination' FORM to 5 parts;
 1. its name, 2. lambda list, 3. list of method-group-specifiers, 4. options, 5. its body."
    (destructuring-bind (_op name lambda-list (&rest method-group-specifiers) &rest rest)
        define-method-combination-form
      (declare (ignore _op))
      (let (options)
        (when (starts-with :arguments (first rest))
          (push (pop rest) options))
        (when (starts-with :generic-function (first rest))
          (push (pop rest) options))
        (nreversef options)
        (values name lambda-list method-group-specifiers options rest))))

  (defun parse-defmethod-form (defmethod-form)
    "Destructs `defmethod' FORM to 4 parts;
 1. its name, 2. list of method-qualifiers, 3. lambda list, 4. its body."
    (destructuring-bind (_op name &rest rest) defmethod-form
      (declare (ignore _op))
      (let* ((method-qualifiers
               (loop for i = (first rest)
                     while (and (atom i) (not (null i)))
                     collect (pop rest)))
             (lambda-list (pop rest)))
        (values name method-qualifiers lambda-list rest))))

  (defgeneric expand-add-declaration-using-head (operator decl-specifier form)
    (:documentation "Called by `expand-add-declaration' to insert DECL-SPECIFIER into FORM.
If FORM can be expanded, returns the expansion. If not, returns FORM.")
    (:method (operator decl-specifier form)
      (if-let ((body-location (operator-body-location operator)))
        (insert-declaration-to-nth-body body-location form decl-specifier
                                        :documentation (operator-accept-docstring-in-body-p operator))
        form))
    (:method ((operator (eql 'defgeneric)) decl-specifier form)
      "For `defgeneric'. Note: This does not add declarations into methods defined in this form."
      (cond
        ((not (starts-with 'optimize decl-specifier))
         (when *at-macro-verbose*
           (warn 'at-macro-style-warning
                 :message (format nil "`defgeneric' accepts only `optimize' declarations.")
                 :form form))
         form)
        (t
         (destructuring-bind (op function-name gf-lambda-list &rest option)
             form
           `(,op ,function-name ,gf-lambda-list (declare ,decl-specifier) ,@option)))))
    (:method ((operator (eql 'define-method-combination)) decl-specifier form)
      (cond
        ((or (<= (length form) 2)       ; shortest form.
             (keywordp (third form)))   ; has a short-form option.
         (when *at-macro-verbose*
           (warn 'at-macro-style-warning
                 :message "The short-form of `define-method-combination' doesn't take declarations."
                 :form form))
         form)
        (t
         (multiple-value-bind (name lambda-list method-group-specifiers options body)
             (parse-define-method-combination-long-form form)
           `(,operator ,name ,lambda-list (,@method-group-specifiers)
                       ,@options
                       ,@(insert-declaration-to-body body decl-specifier
                                                     :whole form :documentation t))))))
    (:method ((operator (eql 'defmethod)) decl-specifier form)
      (multiple-value-bind (name method-qualifiers lambda-list body)
          (parse-defmethod-form form)
        `(,operator ,name ,@method-qualifiers ,lambda-list
                    ,@(insert-declaration-to-body body decl-specifier
                                                  :whole form :documentation t))))
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
  (multiple-value-bind (expansion expanded-p)
      (apply-at-macro `(add-declaration ,decl-specifier)
                      (alexandria:curry #'expand-add-declaration decl-specifier)
                      body env)
    (values (if expanded-p
                expansion
                `(locally (declare ,decl-specifier)
                   ,@body))
            t)))

;;; Declaration only -- `ignore', `ignorable', `dynamic-extent'

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ignore-name-p (x)
    (typecase x
      (symbol t)
      (cons (starts-with 'function x))
      (otherwise nil)))

  (defun %expand-at-declaration (declaration-name name-or-names body)
    (let* ((names (ensure-list-with name-or-names #'ignore-name-p))
           (decl-specifier `(,declaration-name ,@names)))
      (if body
          `(add-declaration ,decl-specifier ,@body)
          `'(declare ,decl-specifier)))))

(defmacro cl-annot-revisit:ignore (name-or-names &body body)
  "Adds `cl:ignore' declaration into BODY.
If BODY is nil, it is expanded to '(declare (ignore ...)), to embed it as a declaration using '#.'"
  (%expand-at-declaration 'cl:ignore name-or-names body))

(defmacro cl-annot-revisit:ignorable (name-or-names &body body)
  "Adds `ignorable' declaration into BODY.
If BODY is nil, it is expanded to '(declare (ignorable ...)), to embed it as a declaration using '#.'"
  (%expand-at-declaration 'cl:ignorable name-or-names body))

(defmacro cl-annot-revisit:dynamic-extent (name-or-names &body body)
  "Adds `dynamic-extent' declaration into BODY.
If BODY is nil, it is expanded to '(declare (dynamic-extent ...)), to embed it as a declaration using '#.'"
  (%expand-at-declaration 'cl:dynamic-extent name-or-names body))
