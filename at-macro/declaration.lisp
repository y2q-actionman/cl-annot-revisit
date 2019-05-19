(in-package :cl-annot-revisit/at-macro)

(define-condition at-declaration-style-warning (at-macro-style-warning)
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *operator-body-location-alist*
    (append '((locally . 1))
            (mapcar
             (lambda (op) (cons op 2))
             '(with-hash-table-iterator with-package-iterator
               flet labals let let* macrolet prog prog* symbol-macrolet ; let-like
               do-all-symbols do-external-symbols do-symbols dolist dotimes ; dolist-like
               lambda pprint-logical-block
               with-input-from-string with-open-file with-open-stream with-output-to-string))
            (mapcar
             (lambda (op) (cons op 3))
             '(destructuring-bind
               define-compiler-macro define-setf-expander defmacro deftype defun ; defun-like
               do do*                   ; do-like
               multiple-value-bind
               with-accessors with-slots))))
  
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

  (defun insert-declaration-to-nth-body (body-index form decl-specifier &key documentation whole)
    (let* ((body (nthcdr body-index form))
           (head (ldiff form body)))
      `(,@head
        ,@(insert-declaration-to-body body decl-specifier
                                      :documentation documentation :whole whole))))


  (defgeneric insert-declaration-1* (form-head form decl-specifier)
    (:documentation "Called by `insert-declaration-1' to insert DECL-SPECIFIER into FORM.
If FORM can be expanded, returns its expansion. If not, returns nil.")
    (:method (form-head form decl-specifier)
      "The bottom case, returns nil."
      (declare (ignore form-head form decl-specifier))
      nil))

  (defmethod insert-declaration-1* :before ((form-head symbol) form decl-specifier)
    (declare (ignore decl-specifier))         
    (when (operator-take-local-declaration-p form-head)
      (warn 'at-declaration-style-warning
            :message (format nil "Adding declarations into ~A form does not works for local declarations"
                             form-head) 
            :form form)))

  (defmethod insert-declaration-1* ((form-head symbol) form decl-specifier)
    "General case."
    (if-let ((body-location (operator-body-location form-head)))
      (insert-declaration-to-nth-body body-location form decl-specifier
                                      :documentation (operator-accept-docstring-in-body-p form-head)
                                      :whole form)))

  (defmethod insert-declaration-1* ((form-head (eql 'defgeneric)) form decl-specifier)
    (destructuring-bind (op function-name gf-lambda-list &rest option)
        form
      `(,op ,function-name ,gf-lambda-list (declare ,decl-specifier) ,@option)))

  (defmethod insert-declaration-1* ((form-head (eql 'define-method-combination)) form decl-specifier)
    (if (<= (length form) 3)
        (warn 'at-declaration-style-warning
              :message "The short-form of `define-method-combination' doesn't take declarations."
              :form form)
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

  (defmethod insert-declaration-1* ((form-head (eql 'defmethod)) form decl-specifier)
    (destructuring-bind (op name &rest rest) form
      (let* ((method-qualifier (if (not (listp (first rest)))
                                   (pop rest)))
             (lambda-list (pop rest)))
        `(,op ,name ,@(if method-qualifier `(,method-qualifier)) ,lambda-list
              ,@(insert-declaration-to-body rest decl-specifier
                                            :whole form :documentation t)))))

  (defmethod insert-declaration-1* ((form-head (eql 'defsetf)) form decl-specifier)
    (if (or (<= (length form) 3)
            (stringp (fourth form)))
        (warn 'at-declaration-style-warning
              :message "The short-form of `defsetf' does not take declarations."
              :form form)
        (insert-declaration-to-nth-body 4 form decl-specifier :whole form :documentation t)))

  ;; They are easy, but are they meaningful?
  ;;   (@inline (func-a) (declaim)) ; => (declaim (inline func-a))
  (defmethod insert-declaration-1* ((form-head (eql 'declaim)) form decl-specifier)
    `(,form-head ,decl-specifier ,@(rest form)))

  (defmethod insert-declaration-1* ((form-head (eql 'proclaim)) form decl-specifier)
    `(,form-head ,decl-specifier ,@(rest form)))


  (defun insert-declaration-1 (form decl-specifier)
    "Insert DECL-SPECIFIER into FORM.
If expansion successed, returns (values <expansion> t).
If failed, returns (values <original-form> nil)."
    (typecase form
      (cons
       (if-let ((expansion (insert-declaration-1* (first form) form decl-specifier)))
         (values expansion t)
         (values form nil)))
      (otherwise (values form nil)))))


;;; Proclamation only -- `declaration'.

(defmacro @declaration (&rest names)
  "Just a shorthand of (declaim (declaration ...))"
  `(declaim (declaration ,@names)))

;;; Declaration only -- `ignore', `ignorable', `dynamic-extent'

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-declaration (new-decl form-head forms environment)
    "If BODY is a form accepts declarations, adds a declaration NEW-DECL into it.
If BODY is nil, it is expanded to (declare NEW-DECL), this is intended to embed it as a declaration using '#.'"
    (cond
      ((not forms)
       `'(declare ,new-decl))
      ((not (length= 1 forms))            ; recursive expansion
       `(progn ,@(apply-to-all-forms form-head forms)))
      (t
       (let ((form (first forms)))
         (mv-cond-let2 (expansion expanded-p)
           ((insert-declaration-1 form new-decl)) ; try known expansions.
           ((apply-to-special-form-1 form-head form)) ; try recursive expansion.
           ((macroexpand-1 form environment) ; try `macroexpand-1'.
            `(values (,@form-head ,@expansion) t))
           (t
            (values form nil))))))))

(defmacro @ignore (variables &body forms &environment env)
  "If BODY is a form accepts declarations, adds `ignore' declaration into it.
If BODY is nil, it is expanded to (declare (ignore ...)), this is intended to embed it as a declaration using '#.'"
  (expand-declaration `(ignore ,@(ensure-list variables))
                      `(@ignore ,variables)
                      forms env))

(defmacro @ignorable (variables &body forms &environment env)
  "If BODY is a form accepts declarations, adds `ignorable' declaration into it.
If BODY is nil, it is expanded to (declare (ignorable ...)), this is intended to embed it as a declaration using '#.'"
  (expand-declaration `(ignorable ,@(ensure-list variables))
                      `(@ignorable ,variables)
                      forms env))

(defmacro @dynamic-extent (variables &body forms &environment env)
  "If BODY is a form accepts declarations, adds `dynamic-extent' declaration into it.
If BODY is nil, it is expanded to (declare (dynamic-extent ...)), this is intended to embed it as a declaration using '#.'"
  (expand-declaration `(dynamic-extent ,@(ensure-list variables))
                      `(@dynamic-extent ,variables)
                      forms env))

;;; Declaration and proclamation -- `type', `inline', `notinline', `ftype', `optimize', `special'

;; TODO: FIXME: wrap multiple forms into `locally'?

;; FIXME: what is '@type' and `the'?


