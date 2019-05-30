(in-package :cl-annot-revisit/at-macro)

(define-condition at-macro-condition (condition)
  ((form :initarg :form)
   (message :initarg :message :initform nil))
  (:report
   (lambda (condition stream)
     (princ (slot-value condition 'message) stream)))
  (:documentation "The root of at-macro  conditions."))

(define-condition at-macro-style-warning (style-warning at-macro-condition)
  ()
  (:documentation "Signaled if some bad styles are found."))

(define-condition at-macro-error (error at-macro-condition)
  ()
  (:documentation "Raised if an error occured."))

(defvar *at-macro-verbose* nil
  "When true, at-macros raise many style-warnings.")
