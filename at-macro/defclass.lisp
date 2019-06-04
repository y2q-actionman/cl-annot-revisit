(in-package :cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun split-defclass-form (form)
    (destructuring-bind (op class-name (&rest superclass-names)
                            (&rest slot-specifiers)
                            &rest class-options)
        form
      (values op class-name superclass-names
              (mapcar #'ensure-list slot-specifiers)
              class-options))))

;;; `@metaclass'
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric expand-@metaclass-1* (form-op form metaclass)
    (:method (form-op form metaclass)
      (declare (ignore form-op form metaclass))
      nil)
    (:method ((form-op (eql 'defclass)) form metaclass)
      (multiple-value-bind (op class-name superclass-names slot-specifiers class-options)
          (split-defclass-form form)
        (when-let (old-metaclass (assoc :metaclass class-options))
          (error 'at-macro-error :form form
                :message (format nil "Metaclass ~A already exists." (cdr old-metaclass))))
        `(,op ,class-name (,@superclass-names)
              (,@slot-specifiers)
              (:metaclass ,metaclass)
              ,@class-options))))

  (defun expand-@metaclass-1 (form metaclass)
    (try-macroexpand
     (if (consp form)
         (expand-@metaclass-1* (first form) form metaclass))
     form)))

(defmacro @metaclass (class-name &body forms &environment env)
  (apply-at-macro `(@metaclass ,class-name)
                  (lambda (form) (expand-@metaclass-1 form class-name))
                  forms env))

;;; `@export-slots'
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric expand-@export-slots-1* (form-op form)
    (:method (form-op form)
      (declare (ignore form-op form))
      nil)
    (:method ((form-op (eql 'defclass)) form)
      (loop with slot-specifiers = (nth-value 3 (split-defclass-form form))
         for (slot-name) in slot-specifiers
         collect slot-name into names
         finally (return
                   (add-export names form))))
    (:method ((form-op (eql 'define-condition)) form)
      (expand-@export-slots-1* 'defclass form)))

  (defun expand-@export-slots-1 (form)
    (try-macroexpand
     (if (consp form)
         (expand-@export-slots-1* (first form) form))
     form)))

(defmacro @export-slots (&body forms &environment env)
  (apply-at-macro '(@export-slots) #'expand-@export-slots-1 forms env))

;;; `@export-accessors'
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *slot-accessor-option-names*
    '(:reader :writer :accessor))
  
  (defgeneric expand-@export-accessors-1* (form-op form)
    (:method (form-op form)
      (declare (ignore form-op form))
      nil)
    (:method ((form-op (eql 'defclass)) form)
      (loop with slot-specifiers = (nth-value 3 (split-defclass-form form))
         for (nil . slot-options) in slot-specifiers
         nconc (loop for (option-name value) on slot-options by #'cddr
                  ;; TODO: FIXME:
                  ;; If this class was extended by metaclass, it may
                  ;; has more keywords making an accessor. The correct
                  ;; way to seeing it is accessing the class object, but
                  ;; it does not exist because this macro works before
                  ;; `defclass' works!
                  ;; My only idea is leaving `*slot-accessor-option-names*'
                  ;; for this purpose...
                  when (member option-name *slot-accessor-option-names*)
                  collect
                    (etypecase value
                      (symbol value)
                      (cons (unless (function-name-p value) ; `:writer' may take a function-name.
                              (warn 'at-macro-style-warning :form form
                                    :message (format nil "~A is not a function name." value)))
                            (second value))))
         into accessors
         finally (return
                   (add-export accessors form))))
    (:method ((form-op (eql 'define-condition)) form)
      (expand-@export-accessors-1* 'defclass form)))

  (defun expand-@export-accessors-1 (form)
    (try-macroexpand
     (if (consp form)
         (expand-@export-accessors-1* (first form) form))
     form)))

(defmacro @export-accessors (&body forms &environment env)
  (apply-at-macro '(@export-accessors) #'expand-@export-accessors-1 forms env))

(defmacro @export-class (&body forms)
  "Just an alias of nested `@export-slots', `@export-accessors', and `@export'."
  `(@export-slots (@export-accessors (@export ,@forms))))
