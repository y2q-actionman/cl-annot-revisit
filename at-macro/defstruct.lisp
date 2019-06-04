(in-package :cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-defstruct-option (name-and-options)
    (let* ((name-and-options (ensure-list name-and-options))
           (name (first name-and-options))
           (options-list (rest name-and-options))
           (options-table (make-hash-table :test 'eq)))
      (flet ((set-it (key val)
               (setf (gethash key options-table) val))
             (push-it (key val)
               (push val (gethash key options-table)))
             ;; They are functions to suppress needless `intern' till used.
             (default-conc-name ()
               (symbolicate name #\-))
             (default-constructor ()
               (list (symbolicate "MAKE-" name))) ; arglist is not supplied.
             (default-copier ()
               (symbolicate "COPY-" name))
             (default-predicate ()
               (symbolicate name "-P")))
        ;; parse options
        (dolist (option options-list)
          (flet ((parse-conc-name-like-option (o-name)
                   (cond ((or (eq option o-name)
                              (equal option `(,o-name)))
                          (set-it o-name nil)
                          t)
                         ((starts-with o-name option)
                          (set-it o-name (second option))
                          t)
                         (t nil))))
            (cond
              ;; :conc-name
              ((parse-conc-name-like-option :conc-name))
              ;; :constructor ; It is a list on options-table because it may appear twice or more.
              ((or (eq option :constructor)
                   (equal option '(:constructor)))
               (push-it :constructor (default-constructor)))
              ((starts-with :constructor option)
               (push-it :constructor (rest option)))
              ;; :copier
              ((parse-conc-name-like-option :copier))
              ;; :include
              ((starts-with :include option)
               (assert (gethash :include options-table) ()
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
              ((parse-conc-name-like-option :predicate))
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
               (set-it :type (second option))))))
        ;; Checks and set defaults
        (ensure-gethash :conc-name options-table (default-conc-name))
        (ensure-gethash :constructor options-table (list (default-constructor)))
        (ensure-gethash :copier options-table (default-copier))
        (when (gethash :initial-offset options-table)
          (assert (not (gethash :type options-table)) ()
                  'at-macro-error :form name-and-options
                  :message ":initial-offset appered but no :type supplied"))
        ;; about `:predicate'
        (let ((named? (or (gethash :named options-table)
                          (not (gethash :type options-table)))))
          (cond ((gethash :predicate options-table)
                 (assert named? ()
                         'at-macro-error :form name-and-options
                         :message ":predicate specified for struct is not named."))
                ((not named?))          ; nop
                (t (set-it :predicate (default-predicate)))))
        (assert (not (and (gethash :print-function options-table)
                          (gethash :print-object options-table)))
                () 'at-macro-error :form name-and-options
                :message ":print-function and :print-object are exclusive."))
      ;; Done
      (values name options-table)))

  (defun parse-defstruct-form (form)
    (let ((operator (pop form)))
      (unless (eq operator 'defstruct)
        (when *at-macro-verbose*
          (warn 'at-macro-style-warning :form form
                :message (format nil "operator ~A is not defstruct" operator)))))
    (multiple-value-bind (name options)
        (parse-defstruct-option (pop form))
      (let* ((documentation (if (stringp (first form))
                                (pop form)
                                nil))
             (slot-descriptions (mapcar #'ensure-list form)))
        (values name options slot-descriptions documentation))))

  (defun pick-names-of-defstruct-form (form kinds)
    (multiple-value-bind (struct-name options slot-descriptions)
        (parse-defstruct-form form)
      (when (gethash :include options)
        ;; If `:include' specified, `defstruct' makes accessors about the included
        ;; struct. I think looking them by `@export-accessors' is very hard...
        (warn 'at-macro-style-warning :form form
              :message "at-macro does not export accessors for :include'd slots."))
      (let (ret)
        (when (member :structure-name kinds) 
          (when (and (gethash :type options)
                     (not (gethash :named options))
                     *at-macro-verbose*)
            (warn 'at-macro-style-warning :form form
                  :message "Unnamed defstruct does not define its name as a type-specifier"))
          (push struct-name ret))
        (when (member :constructor kinds) 
          (loop for (name) in (gethash :constructor options)
             when name
             do (push name ret)))
        (when (member :copier kinds) 
          (push (gethash :copier options) ret))
        (when (member :predicate kinds) 
          (when-let ((p-name (gethash :predicate options)))
            (push p-name ret)))
        (when (intersection '(:slot-name :reader) kinds)
          (loop with conc-name = (gethash :conc-name options)
             for (s-name) in slot-descriptions
             when (member :slot-name kinds)
             do (push s-name ret)
             when (member :reader kinds)
             do (push (if conc-name
                          (symbolicate conc-name s-name)
                          s-name)
                      ret)))
        (nreverse ret)))))


;;; `@export-accessors'
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod expand-@export-accessors-1* ((form-op (eql 'defstruct)) form)
    (let ((readers (pick-names-of-defstruct-form form '(:reader))))
      (add-export readers form))))


;;; `@export-constructors'
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric expand-@export-constructors-1* (form-op form)
    (:method (form-op form)
      (declare (ignore form-op form))
      nil)
    (:method ((form-op (eql 'defstruct)) form)
      (let ((constructors (pick-names-of-defstruct-form form '(:constructor))))
        (add-export constructors form))))

  (defun expand-@export-constructors-1 (form)
    (try-macroexpand
     (if (consp form)
         (expand-@export-constructors-1* (first form) form))
     form)))

(defmacro @export-constructors (&body forms &environment env)
  (apply-at-macro '(@export-constructors) #'expand-@export-constructors-1
                  forms env))


;;; `@export-structure'
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric expand-@export-structure-1* (form-op form)
    (:method (form-op form)
      (declare (ignore form-op form))
      nil)
    (:method ((form-op (eql 'defstruct)) form)
      (let ((all (pick-names-of-defstruct-form
                  form
                  '(:structure-name :constructor :copier :predicate :slot-name :reader))))
        (add-export all form))))

  (defun expand-@export-structure-1 (form)
    (try-macroexpand
     (if (consp form)
         (expand-@export-structure-1* (first form) form))
     form)))

(defmacro @export-structure (&body forms &environment env)
  ;; In original, Just an alias of nested `@export-accessors',`@export-constructors',
  ;; `@export-slots', and `@export'. (But `@export-slots' does nothing).
  (apply-at-macro '(@export-structure) #'expand-@export-structure-1 forms env))
