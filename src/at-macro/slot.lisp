(in-package #:cl-annot-revisit/at-macro)

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
  ((slot-name :initarg :slot-name :initform nil
              :reader at-required-precondition-error-slot-name))
  (:documentation "Raised when :initform supplied for `cl-annot-revisit:required' slot.")
  (:report
   (lambda (condition stream)
     (with-slots (slot-name) condition
       (format stream "Required slot ~A must not have :initform" slot-name)))))

(define-condition at-required-runtime-error (simple-error)
  ((slot-name :initarg :slot-name :initform nil
              :reader at-required-runtime-error-slot-name)
   (initarg :initarg :initarg :initform nil
            :reader at-required-runtime-error-initarg))
  (:documentation "Raised when no value supplied for `cl-annot-revisit:required' slot.")
  (:report
   (lambda (condition stream)
     (with-slots (slot-name initarg) condition
       (format stream "Must supply ~A slot ~@[with :initarg ~A~]"
               slot-name initarg)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-new-value ()
    (format t "Enter a new value: ")
    (list (read)))

  (defun raise-required-slot-error (slot-name initarg-name)
    (restart-case
        (error 'at-required-runtime-error
               :slot-name slot-name :initarg initarg-name)
      (use-value (new-value)
        :report "Use a new value."
        :interactive read-new-value  
        new-value))))

(defmacro cl-annot-revisit:required (slot-specifier)
  "Works like as '@required' of cl-annot. This is intended to used with '#.'"
  (multiple-value-bind (name options)
      (split-slot-specifier slot-specifier)
    (unless (get-properties options '(:initarg))
      (setf options (list* :initarg (make-keyword name) options)))
    (when (get-properties options '(:initform))
      (error 'at-required-precondition-error :slot-name name))
    (setf options
          (list* :initform `(raise-required-slot-error ',name ',(getf options :initarg))
                 options))
    `'(,name ,@options)))
