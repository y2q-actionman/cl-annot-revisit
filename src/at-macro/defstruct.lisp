(in-package #:cl-annot-revisit-at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-defstruct-option (name-and-options)
    (let* ((name-and-options (ensure-list name-and-options))
           (name (first name-and-options))
           (options-list (rest name-and-options))
           (options-table (make-hash-table :test 'eq))
           (default-conc-name (symbolicate name #\-))
           (default-constructor (list (symbolicate "MAKE-" name))) ; arglist is not supplied.
           (default-copier (symbolicate "COPY-" name))
           (default-predicate (symbolicate name "-P")))
      (flet ((set-it (key val)
               (setf (gethash key options-table) val))
             (push-it (key val)
               (push val (gethash key options-table)))
             (zero-arg-option-p (name option)
               (or (eq option name)
                   (equal option `(,name)))))
        ;; parse options
        (dolist (option options-list)
          (cond
            ;; :conc-name
            ((zero-arg-option-p :conc-name option)
             (set-it :conc-name nil)) ; If nil or no arg, no prefix is used.
            ((starts-with :conc-name option)
             (set-it :conc-name (second option)))
            ;; :constructor ; It is a list on options-table because it may appear twice or more.
            ((zero-arg-option-p :constructor option)
             (push-it :constructor default-constructor))
            ((starts-with :constructor option)
             (push-it :constructor (rest option)))
            ;; :copier
            ((zero-arg-option-p :copier option)
             (set-it :copier default-copier)) ; if no argument, uses default
            ((starts-with :copier option)
             (set-it :copier (second option)))
            ;; :include
            ((starts-with :include option)
             (assert (not (gethash :include options-table)) ()
                     'at-macro-error :form name-and-options
                     :message "Two :include options appeared.")
             (set-it :include (rest option)))
            ;; :initial-offset
            ((starts-with :initial-offset option)
             (let ((offset (second option)))
               (assert (typep offset '(integer 0)) ()
                       'at-macro-error :form name-and-options
                       :message ":initial-offset must be a non-zero integer")
               (set-it :initial-offset offset)))
            ;; :named
            ((eq option :named)
             (set-it :named t))
            ;; :predicate
            ((zero-arg-option-p :predicate option)
             (set-it :predicate default-predicate))
            ((starts-with :predicate option)
             (set-it :predicate (second option)))
            ;; :print-function
            ((starts-with :print-function option)
             (set-it :print-function
                     ;; If no name and :named, `print-object' will be defined.
                     ;; I assign T on this situation.
                     (if (length= 1 option) t (second option))))
            ;; :print-object
            ((starts-with :print-object option)
             (set-it :print-object (if (length= 1 option) t (second option))))
            ;; :type
            ((starts-with :type option)
             (set-it :type (second option)))
            ;; 
            (t (error 'at-macro-error :form name-and-options
                      :message (format nil "Unknown defstruct option ~A" option)))))
        ;; Checks and set defaults
        (ensure-gethash :conc-name options-table default-conc-name)
        (ensure-gethash :constructor options-table (list default-constructor))
        (nreversef (gethash :constructor options-table))
        (ensure-gethash :copier options-table default-copier)
        (when (gethash :initial-offset options-table)
          (assert (gethash :type options-table) ()
                  'at-macro-error :form name-and-options
                  :message ":initial-offset appered but no :type supplied"))
        ;; about `:predicate'
        (let ((named? (or (gethash :named options-table)
                          (not (gethash :type options-table))))
              (predicate-option-exists?
                (nth-value 1 (gethash :predicate options-table))))
          (cond
            (predicate-option-exists?
             (assert named? ()
                     'at-macro-error :form name-and-options
                                     :message ":predicate specified for struct is not named."))
            ((not named?))              ; nop
            (t (set-it :predicate default-predicate))))
        (assert (not (and (gethash :print-function options-table)
                          (gethash :print-object options-table)))
                () 'at-macro-error :form name-and-options
                :message ":print-function and :print-object are exclusive."))
      ;; Done
      (values name options-table)))

  (defun parse-defstruct-form (form)
    (let ((operator (pop form)))
      (when (and *at-macro-verbose*
                 (not (eq operator 'cl:defstruct)))
        (warn 'at-macro-style-warning :form form
              :message (format nil "operator ~A is not defstruct" operator))))
    (multiple-value-bind (name options)
        (parse-defstruct-option (pop form))
      (let* ((documentation (if (stringp (first form))
                                (pop form)
                                nil))
             (slot-descriptions (mapcar #'ensure-list form)))
        (values name options slot-descriptions documentation))))

  (defun pick-names-of-defstruct-form
      (form &optional (kinds '(:structure-name :constructor :copier :predicate :accessor)))
    (multiple-value-bind (struct-name options slot-descriptions)
        (parse-defstruct-form form)
      (let (ret)
        (when (member :structure-name kinds) 
          (when (and *at-macro-verbose*
                     (gethash :type options)
                     (not (gethash :named options)))
            (warn 'at-macro-style-warning :form form
                  :message "Unnamed defstruct does not define its name as a type-specifier"))
          (pushnew struct-name ret))
        (when (member :constructor kinds) 
          (loop for (name) in (gethash :constructor options)
             when name
             do (pushnew name ret)))
        (when (member :copier kinds)
          (when-let ((c-name (gethash :copier options)))
            (pushnew c-name ret)))
        (when (member :predicate kinds) 
          (when-let ((p-name (gethash :predicate options)))
            (pushnew p-name ret)))
        (when (member :accessor kinds)
          (when (gethash :include options)
            ;; If `:include' specified, `defstruct' makes accessors about the included
            ;; struct. I think looking them by `cl-annot-revisit:export-accessors' is very hard...
            ;; This warning cannot be suppressed by setting `*at-macro-verbose*' intentionally.
            (warn 'at-macro-style-warning :form form
                  :message "at-macro does not export accessors for :include'd slots."))
          (loop with conc-name = (gethash :conc-name options)
             for (s-name . nil) in slot-descriptions
             do (pushnew (if conc-name
                             (symbolicate conc-name s-name)
                             s-name)
                         ret)))
        (nreverse ret)))))

;;; for `find-name-to-be-defined'
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod find-name-to-be-defined-using-head ((form-head (eql 'cl:defstruct)) form)
    "Special handling for `defstruct'. Its second form may contain some options."
    (nth-value 0 (parse-defstruct-form form))))

;;; for `cl-annot-revisit:documentation'
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod expand-documentation-using-head ((operator (eql 'defstruct)) docstring form)
    "Special handling for `defstruct', which define a new type only when it doesn't have :type."
    (multiple-value-bind (name options)
        (parse-defstruct-form form)
      (declare (ignore name))
      (with-gensyms (obj)
        `(let ((,obj ,form))
           (setf (documentation ,obj 'structure) ,docstring
                 ,@(unless (gethash :type options)
                     `((documentation ,obj 'type) ,docstring)))
           ,obj)))))

;;; `export-accessors'
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod expand-export-accessors-using-head ((form-op (eql 'defstruct)) form)
    (let ((readers (pick-names-of-defstruct-form form '(:accessor))))
      (add-export readers form))))

;;; `export-constructors'
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric expand-export-constructors-using-head (form-op form)
    (:method (form-op form)
      (declare (ignore form-op))
      form)
    (:method ((form-op (eql 'defstruct)) form)
      (let ((constructors (pick-names-of-defstruct-form form '(:constructor))))
        (add-export constructors form))))

  (defun expand-export-constructors (form)
    (macroexpand-convention (form)
     (if (consp form)
         (expand-export-constructors-using-head (first form) form)
         form))))

(defmacro cl-annot-revisit:export-constructors (&body forms &environment env)
  "`Export' constructors of the structure will be defined in FORMS."
  (apply-at-macro-for-each-form '(cl-annot-revisit:export-constructors)
                                #'expand-export-constructors forms env))

;;; `export-structure'
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric expand-export-structure-using-head (form-op form)
    (:method (form-op form)
      (declare (ignore form-op))
      form)
    (:method ((form-op (eql 'defstruct)) form)
      (let ((all (pick-names-of-defstruct-form form)))
        (add-export all form))))

  (defun expand-export-structure (form)
    (macroexpand-convention (form)
     (if (consp form)
         (expand-export-structure-using-head (first form) form)
         form))))

(defmacro cl-annot-revisit:export-structure (&body forms &environment env)
  "`Export' the name, constructors, copier, predicate, slot-names and
accessors of the structure will be defined in FORMS."
  ;; In original, Just an alias of nested `@export-accessors',`@export-constructors',
  ;; `@export-slots', and `@export'. (But `@export-slots' does nothing).
  (apply-at-macro-for-each-form '(cl-annot-revisit:export-structure)
                                #'expand-export-structure forms env))
