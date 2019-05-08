(in-package :cl-annot-revisit/at-macro)

;;; Proclamation only

(defmacro @declaration (&rest names)
  "Just a shorthand of (declaim (declaration ...))"
  `(declaim (declaration ,@names)))

;;; Declaration only

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric insert-declaration* (form-head form new-declaration)
    (:documentation "Called by `insert-declaration' to insert NEW-DECLARATION into FORM.")
    (:method (form-head form new-declaration)
      "The bottom case, returns nil."
      (declare (ignore form-head new-declaration))
      nil))

  
  (defun insert-declaration-to-body (form-body new-declaration &key documentation whole)
    (multiple-value-bind (body decls doc)
        (parse-body form-body :documentation documentation :whole whole)
      `(,new-declaration
        ,@decls
        ,@(if doc `(,doc)) 
        ,@body)))

  (defun insert-declaration-to-nth-body (body-index form new-declaration &key documentation whole)
    (let* ((body (nthcdr body-index form))
           (head (ldiff form body)))
      `(,@head
        ,@(insert-declaration-to-body body new-declaration
                                      :documentation documentation :whole whole))))
  

  ;; TODO: support `declaim'?
  
  (defmethod insert-declaration* ((form-head (eql 'defgeneric)) form new-declaration)
    (destructuring-bind (op function-name gf-lambda-list &rest option)
        form
      `(,op ,function-name ,gf-lambda-list ,new-declaration ,@option)))

  (defun insert-declaration-to-defun-like-form (form new-declaration)
    ;; syntax: (op func-name lambda-list &body body)
    (insert-declaration-to-nth-body 3 form new-declaration :documentation t :whole form))

  (defmethod insert-declaration* ((form-head (eql 'define-compiler-macro)) form new-declaration)
    (insert-declaration-to-defun-like-form form new-declaration))

  (defmethod insert-declaration* ((form-head (eql 'define-method-combination))
                                  form new-declaration)
    (if (<= (length form) 3)
        (values form nil) ; The short-form of `define-method-combination' doesn't take declarations.
        (destructuring-bind
              (op name lambda-list (&rest method-group-specifier) &rest rest) form
          (let (options)
            (when (starts-with :arguments (first rest))
              (push (pop rest) options))
            (when (starts-with :generic-function (first rest))
              (push (pop rest) options))
            (nreversef options)
            `(,op ,name ,lambda-list (,@method-group-specifier)
                  ,@options
                  ,@(insert-declaration-to-body rest new-declaration
                                                :whole form :documentation t))))))

  (defmethod insert-declaration* ((form-head (eql 'define-setf-expander))
                                  form new-declaration)
    (insert-declaration-to-defun-like-form form new-declaration))
  
  (defmethod insert-declaration* ((form-head (eql 'defmacro)) form new-declaration)
    (insert-declaration-to-defun-like-form form new-declaration))

  (defmethod insert-declaration* ((form-head (eql 'defmethod)) form new-declaration)
    (destructuring-bind (op name &rest rest) form
      (let* ((method-qualifier (if (not (listp (first rest)))
                                   (pop rest)))
             (lambda-list (pop rest)))
        `(,op ,name ,@(if method-qualifier `(,method-qualifier)) ,lambda-list
              ,@(insert-declaration-to-body rest new-declaration
                                            :whole form :documentation t)))))

  (defmethod insert-declaration* ((form-head (eql 'defsetf)) form new-declaration)
    (if (or (<= (length form) 3)
            (stringp (fourth form)))
        (values form nil) ; The short-form of `defsetf' does not take declarations.
        ;; syntax: (op name lambda-list (&rest store-variable) &body body)
        (insert-declaration-to-nth-body 4 form new-declaration :whole form :documentation t)))
  
  (defmethod insert-declaration* ((form-head (eql 'deftype)) form new-declaration)
    (insert-declaration-to-defun-like-form form new-declaration))

  (defmethod insert-declaration* ((form-head (eql 'defun)) form new-declaration)
    (insert-declaration-to-defun-like-form form new-declaration))

  (defmethod insert-declaration* ((form-head (eql 'destructuring-bind)) form new-declaration)
    ;; syntax: (op lambda-list expr &body body)
    (insert-declaration-to-nth-body 3 form new-declaration :whole form))

  (defun insert-declaration-to-do-form (form new-declaration)
    ;; syntax: (op (&rest vars) (&rest end-tests) &body body)
    (insert-declaration-to-nth-body 3 form new-declaration :whole form))
  
  (defmethod insert-declaration* ((form-head (eql 'do)) form new-declaration)
    (insert-declaration-to-do-form form new-declaration))

  (defmethod insert-declaration* ((form-head (eql 'do*)) form new-declaration)
    (insert-declaration-to-do-form form new-declaration))

  (defun insert-declaration-to-dolist-like-form (form new-declaration)
    ;; syntax: (op (var &rest extras) &body body)
    (insert-declaration-to-nth-body 2 form new-declaration :whole form))
  
  (defmethod insert-declaration* ((form-head (eql 'do-all-symbols)) form new-declaration)
    (insert-declaration-to-do-like-form form new-declaration))
  
  (defmethod insert-declaration* ((form-head (eql 'do-external-symbols)) form new-declaration)
    (insert-declaration-to-do-like-form form new-declaration))
  
  (defmethod insert-declaration* ((form-head (eql 'do-symbols)) form new-declaration)
    (insert-declaration-to-do-like-form form new-declaration))
  
  (defmethod insert-declaration* ((form-head (eql 'dolist)) form new-declaration)
    (insert-declaration-to-do-like-form form new-declaration))
  
  (defmethod insert-declaration* ((form-head (eql 'dotimes)) form new-declaration)
    (insert-declaration-to-do-like-form form new-declaration))

  (defun insert-declaration-to-let-like-form (form new-declaration)
    ;; syntax: (op (&rest bindings) &body body)
    (insert-declaration-to-nth-body 2 form new-declaration :whole form))

  (defmethod insert-declaration* ((form-head (eql 'flet)) form new-declaration)
    ;; FIXME: what to do on local functions?
    (insert-declaration-to-let-like-form form new-declaration))

  ;; FIXME: what to do `hander-case' clauses?
  
  (defmethod insert-declaration* ((form-head (eql 'labels)) form new-declaration)
    ;; FIXME: what to do on local functions?
    (insert-declaration-to-let-like-form form new-declaration))

  (defmethod insert-declaration* ((form-head (eql 'lambda)) form new-declaration)
    ;; syntax: (op lambda-list &body body)
    (insert-declaration-to-nth-body 2 form new-declaration :whole form :documentation t))

  (defmethod insert-declaration* ((form-head (eql 'let)) form new-declaration)
    (insert-declaration-to-let-like-form form new-declaration))

  (defmethod insert-declaration* ((form-head (eql 'let*)) form new-declaration)
    (insert-declaration-to-let-like-form form new-declaration))

  (defmethod insert-declaration* ((form-head (eql 'locally)) form new-declaration)
    ;; syntax: (op &body body)
    (insert-declaration-to-nth-body 1 form new-declaration :whole form))

  (defmethod insert-declaration* ((form-head (eql 'macrolet)) form new-declaration)
    ;; FIXME: what to do on local functions?
    (insert-declaration-to-let-like-form form new-declaration))

  (defmethod insert-declaration* ((form-head (eql 'multiple-value-bind)) form new-declaration)
    ;; syntax: (op (&rest var) values-form &body body)
    (insert-declaration-to-nth-body 3 form new-declaration :whole form))

  (defmethod insert-declaration* ((form-head (eql 'pprint-logical-block)) form new-declaration)
    ;; syntax: (op (&rest options) &body body)
    (insert-declaration-to-nth-body 2 form new-declaration :whole form))

  ;; TODO: support `proclaim'?
  
  (defmethod insert-declaration* ((form-head (eql 'prog)) form new-declaration)
    (insert-declaration-to-let-like-form form new-declaration))

  (defmethod insert-declaration* ((form-head (eql 'prog*)) form new-declaration)
    (insert-declaration-to-let-like-form form new-declaration))

  ;; FIXME: what to do `restart-case' clauses?
  
  (defmethod insert-declaration* ((form-head (eql 'symbol-macrolet)) form new-declaration)
    (insert-declaration-to-let-like-form form new-declaration))

  (defmethod insert-declaration* ((form-head (eql 'with-accessors)) form new-declaration)
    ;; syntax: (op (&rest accessors) obj &body body)
    (insert-declaration-to-nth-body 3 form new-declaration :whole form))

  (defmethod insert-declaration* ((form-head (eql 'with-hash-table-iterator)) form new-declaration)
    (insert-declaration-to-dolist-like-form form new-declaration))

  (defmethod insert-declaration* ((form-head (eql 'with-input-from-string)) form new-declaration)
    ;; syntax: (op (&rest vars) &body body)
    (insert-declaration-to-nth-body 2 form new-declaration :whole form))

  (defmethod insert-declaration* ((form-head (eql 'with-open-file)) form new-declaration)
    ;; syntax: (op (&rest vars) &body body)
    (insert-declaration-to-nth-body 2 form new-declaration :whole form))

  (defmethod insert-declaration* ((form-head (eql 'with-open-stream)) form new-declaration)
    ;; syntax: (op (&rest vars) &body body)
    (insert-declaration-to-nth-body 2 form new-declaration :whole form))

  (defmethod insert-declaration* ((form-head (eql 'with-output-to-string)) form new-declaration)
    ;; syntax: (op (&rest vars) &body body)
    (insert-declaration-to-nth-body 2 form new-declaration :whole form))

  (defmethod insert-declaration* ((form-head (eql 'with-package-iterator)) form new-declaration)
    (insert-declaration-to-dolist-like-form form new-declaration))

  (defmethod insert-declaration* ((form-head (eql 'with-slots)) form new-declaration)
    ;; syntax: (op (&rest accessors) obj &body body)
    (insert-declaration-to-nth-body 3 form new-declaration :whole form))
  

  (defun insert-declaration (form new-declaration)
    "Insert NEW-DECLARATION into FORM.
If expansion successed, returns (values <expansion> t).
If failed, returns (values <original-form> nil)."
    (assert (starts-with 'declare new-declaration))
    (check-type form list)      ; fixme: What to do if a symbol-macro?
    (if-let ((expansion (insert-declaration* (first form) form new-declaration)))
      (values expansion t)
      (values form nil))))

(defmacro @ignore (variables &body forms &environment env)
  "If BODY is a form accepts declarations, adds `ignore' declaration into it.
If BODY is nil, it is expanded to (declare (ignore ...)), this is intended to embed as a declaration using '#.'"
  (let ((new-decl `(declare (ignore ,@(ensure-list variables)))))
    (cond
      ((not forms)
       `',new-decl)
      ((not (length= 1 forms))          ; recursive expansion
       `(progn ,@(add-to-all-heads '@ignore forms)))
      (t
       (let ((form (first forms)))
         (mv-cond-let2 (expansion expanded-p)
           ((insert-declaration form new-decl)) ; try known expansions.
           ;; FIXME: I must pass `(@ignore ,variables)
           ;; ((apply-to-special-form-1 '@export form)) ; try recursive expansion.
           ((macroexpand-1 form env))   ; try `macroexpand-1'.
           (t                     ; nothing to do. return FORM itself.
            (values form nil))))))))

;; FIXME: what is '@type' and `the'?
