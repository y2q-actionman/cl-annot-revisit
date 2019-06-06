(in-package #:cl-annot-revisit/at-macro)

;;; `@metaclass'
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pick-defclass-options (defclass-form)
    (nthcdr 4 defclass-form))
  
  (defgeneric expand-@metaclass-1* (form-op form metaclass)
    (:method (form-op form metaclass)
      (declare (ignore form-op form metaclass))
      nil)
    (:method ((form-op (eql 'defclass)) form metaclass)
      (let ((class-options (pick-defclass-options form)))
        (when (assoc :metaclass class-options)
          (error 'at-macro-error :form form
                 :message "A metaclass already exists."))
        `(,@form (:metaclass ,metaclass)))))

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
  (defun pick-defclass-slots (defclass-form)
    (mapcar #'ensure-list (nth 3 defclass-form)))
  
  (defgeneric expand-@export-slots-1* (form-op form)
    (:method (form-op form)
      (declare (ignore form-op form))
      nil)
    (:method ((form-op (eql 'defclass)) form)
      (loop with slot-specifiers = (pick-defclass-slots form)
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
      (when (assoc :metaclass (pick-defclass-options form))
        ;; TODO: FIXME:
        ;; If this class was extended by metaclass, it may has more
        ;; keywords making an accessor. The correct way to seeing it is
        ;; accessing the class object, but it does not exist because
        ;; this macro works before `defclass' works!
        ;; My only idea is leaving `*slot-accessor-option-names*' for
        ;; this purpose...
        (when *at-macro-verbose*
          (warn 'at-macro-style-warning :form form
                :message "Additional slot options added by metaclass is ignored, if it is not in *slot-accessor-option-names*.")))
      (loop with slot-specifiers = (pick-defclass-slots form)
         for (nil . slot-options) in slot-specifiers
         nconc (loop for (option-name value) on slot-options by #'cddr
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
