(in-package :cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-defstruct-option (name-or-options &optional (*package* *package*))
    (let* ((name-and-options (ensure-list name-or-options))
           (name (first name-and-options))
           (options-list (rest name-and-options))
           (options-table (make-hash-table)))
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
               (when (gethash :include options-table)
                 (error "Two :include options appeared."))
               (set-it :include (rest option)))
              ;; :initial-offset
              ((starts-with :initial-offset option)
               (let ((offset (second option)))
                 (check-type offset '(integer 0))
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
        (assert (and (gethash :initial-offset options-table)
                     (not (gethash :type options-table)))
                () ":initial-offset appered but no :type supplied")
        ;; abount `:predicate'
        (let ((named? (or (gethash :named options-table)
                          (not (gethash :type options-table)))))
          (cond ((gethash :predicate options-table)
                 (assert named? () ":predicate specified for struct is not named."))
                ((not named?))          ; nop
                (t (set-it :predicate (default-predicate)))))
        (assert (not (and (gethash :print-function options-table)
                          (gethash :print-object options-table)))
                () ":print-function and :print-object are exclusive."))
      ;; Done
      (values name options-table)))

  ;; TODO
  #+ignore
  (defmethod expand-@export-accessors-1* ((form-op (eql 'defstruct)) form)
    )

  ;; TODO: If `:include' specified, `defstruct' makes accessors about the included
  ;; struct. I think looking them by `@export-accessors' is very hard...
  ;; (It will be a style-warning.)

  ;; TODO: about :named
  ;; -- This effects whether the predicate defined or not if `:type' supplied.
  ;; -- but not make a type specifier.

  )

;;; TODO : @export-constructors -- works only for `defstruct'


;; TODO: #:@export-structure
