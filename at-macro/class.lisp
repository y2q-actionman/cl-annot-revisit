(in-package :cl-annot-revisit/at-macro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric expand-at-class-1* (at-macro-name form-operator at-macro-form form)
    (:documentation "Called by `expand-at-class-1' to compute a result.
If FORM can be expanded, returns its expansion. If not, returns nil."))

  (defmethod expand-at-class-1* ((at-macro-name (eql '@export-slots)) (form-op (eql 'defclass))
                                 at-macro-form form)
    (destructuring-bind (_op _class-name (&rest _superclass-names)
                             (&rest slot-specifiers)
                             &rest _class-options)
        form
      (declare (ignore _op _class-name _superclass-names _class-options))
      (let ((slot-names
             (loop for slot-spec in slot-specifiers
                if (symbolp slot-spec)
                collect slot-spec
                else if (listp slot-spec)
                collect (first slot-spec))))
        (if slot-names
            `(progn (export ',slot-names)
                    ,form)
            form))))

  (defmethod expand-at-class-1* ((at-macro-name (eql '@export-slots)) (form-op (eql 'defclass))
                                 at-macro-form form)
    (destructuring-bind (_op _class-name (&rest _superclass-names)
                             (&rest slot-specifiers)
                             &rest _class-options)
        form
      (declare (ignore _op _class-name _superclass-names _class-options))
      (let ((slot-names
             (loop for slot-spec in slot-specifiers
                if (symbolp slot-spec)
                collect slot-spec
                else if (listp slot-spec)
                collect (first slot-spec))))
        (if slot-names
            `(progn (export ',slot-names)
                    ,form)
            form))))

  (defmethod expand-at-class-1* ((at-macro-name (eql '@metaclass)) (form-op (eql 'defclass))
                                 at-macro-form form)
    (destructuring-bind (op class-name (&rest superclass-names)
                             (&rest slot-specifiers)
                             &rest class-options)
        form
      (when-let (metaclass (assoc :metaclass class-options))
        (error 'at-macro-error :form form
               :message (format nil "Metaclass ~A already exists." (cdr metaclass))))
      `(,op ,class-name (,@superclass-names)
            (,@slot-specifiers)
            (:metaclass ,(second at-macro-form))
            ,@class-options)))

  (defun expand-at-class-1 (at-macro-form form)
    "Called by at-macros for class to expand known ones.
If expansion successed, returns (values <expansion> t).
If failed, returns (values FORM nil)."
    (typecase form
      (cons (if-let ((expansion (expand-at-class-1* (first at-macro-form) (first form)
                                                    at-macro-form form)))
              (values expansion t)
              (values form nil)))
      (otherwise (values form nil))))




  )


(defmacro @export-accessors (&body forms &environment env)
  (apply-at-macro '(@export-accessors) forms env))

(defmacro @export-slots (&body forms &environment env)
  (apply-at-macro '(@export-slots) forms env))

(defmacro @metaclass (class-name &body forms &environment env)
  (apply-at-macro `(@metaclass ,class-name) forms env))


   ;; #:@export-accessors
   ;; #:@export-constructors
;; #:@export-class

   ;; #:@export-structure


