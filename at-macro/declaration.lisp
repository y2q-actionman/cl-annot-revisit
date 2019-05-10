(in-package :cl-annot-revisit/at-macro)

;;; Proclamation only

(defmacro @declaration (&rest names)
  "Just a shorthand of (declaim (declaration ...))"
  `(declaim (declaration ,@names)))

;;; Declaration only

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *cl-operator-body-location-1-list*
    '(locally)
    "List of operators its place of body is accesible with (nth 1 body).")

  (defparameter *cl-operator-body-location-2-list*
    '(with-hash-table-iterator with-package-iterator
      flet labals let let* macrolet prog prog* symbol-macrolet ; let-like
      do-all-symbols do-external-symbols do-symbols dolist dotimes ; dolist-like
      lambda pprint-logical-block
      with-input-from-string with-open-file with-open-stream with-output-to-string)
    "List of operators its place of body is accesible with (nth 2 body).")

  (defparameter *cl-operator-body-location-3-list*
    '(destructuring-bind
      define-compiler-macro define-setf-expander defmacro deftype defun ; defun-like
      do do*                         ; do-like
      multiple-value-bind
      with-accessors with-slots)
    "List of operators its place of body is accesible with (nth 3 body).")

  (defun body-location-of-operator (name)
    (cond ((member name *cl-operator-body-location-1-list*) 1)
          ((member name *cl-operator-body-location-2-list*) 2)
          ((member name *cl-operator-body-location-3-list*) 3)
          (t nil)))

  (defparameter *cl-operator-accepts-docstring-in-body-list*
    '(lambda
      define-compiler-macro define-setf-expander defmacro deftype defun ; defun-like
      )
    "List of operators accepts docstring in its body.")

  (defun body-accept-docstring-p (name)
    (member name *cl-operator-accepts-docstring-in-body-list*))
  

  
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


  (defgeneric insert-declaration* (form-head form new-declaration)
    (:documentation "Called by `insert-declaration' to insert NEW-DECLARATION into FORM.")
    (:method (form-head form new-declaration)
      "The bottom case, returns nil."
      (declare (ignore form-head form new-declaration))
      nil))

  (defmethod insert-declaration* ((form-head symbol) form new-declaration)
    "General case."
    (if-let ((body-location (body-location-of-operator form-head)))
      (insert-declaration-to-nth-body body-location form new-declaration
                                      :whole form
                                      :documentation (body-accept-docstring-p form-head))))

  (defmethod insert-declaration* ((form-head (eql 'defgeneric)) form new-declaration)
    (destructuring-bind (op function-name gf-lambda-list &rest option)
        form
      `(,op ,function-name ,gf-lambda-list ,new-declaration ,@option)))

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

  ;; FIXME: what to do on local functions?
  ;; about `flet', `labels', `macrolet'
  
  ;; FIXME: what to do `hander-case' clauses?
  
  ;; TODO: support `declaim'?
  ;; TODO: support `proclaim'?
  
  ;; FIXME: what to do `restart-case' clauses?

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
