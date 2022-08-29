(in-package #:cl-annot-revisit/at-macro)

;;; NOTE:
;;; The macros defined here is derived from cl-annot.  They use a
;;; keyword having a same name of slot-name as :initarg.  However, I
;;; think using it always is not a good style. Additionally, I should
;;; argue whether :initarg is added *ONLY* when no :initarg there.

(defun split-slot-specifier (slot-specifier)
  "Returns the slot name and slot options from SLOT-SPECIFIER."
  (etypecase slot-specifier
    (symbol (values slot-specifier nil))
    (cons (values (first slot-specifier) (rest slot-specifier)))))

(defmacro cl-annot-revisit:optional (initform slot-specifier)
  "Works like as '@optional' of cl-annot. This is intended to used with '#.'"
  (multiple-value-bind (name options)
      (split-slot-specifier slot-specifier)
    (unless (get-properties options '(:initarg))
      (setf options (list* :initarg (make-keyword name) options)))
    (if (get-properties options '(:initform))
        (when *at-macro-verbose*
          (warn 'at-macro-style-warning :form slot-specifier
                :message "cl-annot-revisit:optional's initform is ignored because slot has :initform already"))
        (setf options (list* :initform initform options)))
    `'(,name ,@options)))

(define-condition at-required-precondition-error (simple-error)
  ((slot-name :initarg :slot-name :initform nil))
  (:documentation "Raised when :initform supplied for `cl-annot-revisit:required' slot.")
  (:report
   (lambda (condition stream)
     (with-slots (slot-name) condition
       (format stream "Required slot ~A must not have :initform" slot-name)))))

(define-condition at-required-runtime-error (simple-error)
  ((slot-name :initarg :slot-name :initform nil)
   (initarg :initarg :initarg :initform nil))
  (:documentation "Raised when no value supplied for `cl-annot-revisit:required' slot.")
  (:report
   (lambda (condition stream)
     (with-slots (slot-name initarg) condition
       (format stream "Must supply ~A slot ~@[with :initarg ~A~]"
               slot-name initarg)))))

(defmacro cl-annot-revisit:required (slot-specifier)
  "Works like as '@required' of cl-annot. This is intended to used with '#.'"
  (multiple-value-bind (name options)
      (split-slot-specifier slot-specifier)
    (unless (get-properties options '(:initarg))
      (setf options (list* :initarg (make-keyword name) options)))
    (when (get-properties options '(:initform))
      (error 'at-required-precondition-error :slot-name name)) ; FIXME: change to style-warning?
    (setf options
          (list* :initform
                 ;; TODO: Utilize `use-value' restart.
                 `(cerror "Enter a value."
                          'at-required-runtime-error
                          :slot-name ',name :initarg ',(getf options :initarg))
                 options))
    `'(,name ,@options)))
