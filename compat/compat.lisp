(in-package #:cl-annot-revisit-compat)

;;; Copied from the original cl-annot.
(defpackage #:cl-annot.core
  (:use :cl)
  (:export
    #:annotation-real
    #:annotation-arity
    #:annotation-inline-p
    #:annotation-form
    #:annotation-form-p
    #:%annotation))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod cl-annot-revisit-at-syntax:find-at-syntax-arity
      ((operator symbol) (cl-annot-compatible-p (eql t)))
    (or (get operator 'cl-annot.core:annotation-arity)
        (call-next-method)))

  (defmethod cl-annot-revisit-at-syntax:expand-at-read-time-p
      ((operator symbol) (cl-annot-compatible-p (eql t)))
    (or (get operator 'cl-annot.core:annotation-inline-p)
        (call-next-method)))

  (defmethod cl-annot-revisit-at-syntax:resolve-at-syntax-alias
      ((operator symbol) (cl-annot-compatible-p (eql t)))
    (or (get operator 'cl-annot.core:annotation-real)
        (call-next-method))))

(defmacro defannotation
    (name lambda-list
     (&key (arity (count-lambda-list-required-arguments lambda-list))
        (inline nil inline-supplied-p)
        (alias nil alias-supplied-p))
     &body body)
  "`cl-annot:defannotation' like one."
  (let ((alias-list (ensure-list alias)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'cl-annot.core:annotation-arity) ,arity)
         ,@(when inline-supplied-p
             `((setf (get ',name 'cl-annot.core:annotation-inline-p) ,inline)))
         ,@(when alias-supplied-p
             (loop for alias in alias-list
                   collect
                   `(setf (get ',alias 'cl-annot.core:annotation-real) ',name))))
       (defmacro ,name ,lambda-list
         ,@body))))

;;; In the original cl-annot, some '@' macros are defined as 'inline'
;;; or may have an alias from CL symbols. I follow the convention
;;; below.

;;; Having an alias to the CL package.

(defannotation compat-export (form)
    (:alias cl:export)
  `(cl-annot-revisit:export ,form))

;;; Slots

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %unquote (form)
    "(quote <something>) -> <something>"
    (assert (starts-with 'quote form))
    (second form)))

(defannotation compat-optional (initform slot-specifier)
    (:inline t :alias cl-annot-revisit:optional)
  (%unquote
   (macroexpand `(cl-annot-revisit:optional ,initform ,slot-specifier))))

(defannotation compat-required (slot-specifier)
    (:inline t :alias cl-annot-revisit:required)
  (%unquote
   (macroexpand `(cl-annot-revisit:required ,slot-specifier))))

;; Declarations
  
(defannotation compat-ignore (name-or-names)
    (:inline t :alias (cl-annot-revisit:ignore cl:ignore))
  (%unquote
   (macroexpand `(cl-annot-revisit:ignore ,name-or-names))))

(defannotation compat-ignorable (name-or-names)
    (:inline t :alias (cl-annot-revisit:ignorable cl:ignorable))
  (%unquote
   (macroexpand `(cl-annot-revisit:ignorable ,name-or-names))))

(defannotation compat-dynamic-extent (name-or-names)
    (:inline t :alias (cl-annot-revisit:dynamic-extent cl:dynamic-extent))
  (%unquote
   (macroexpand `(cl-annot-revisit:dynamic-extent ,name-or-names))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %remove-declaim-and-unquote (form)
    "See `cl-annot-revisit-at-macro::expand-to-declaim-form'"
    (assert (starts-with 'progn form))
    (assert (starts-with 'declaim (second form)))
    (%unquote (third form))))

(defannotation compat-special (name-or-names)
    (:inline t :alias (cl-annot-revisit:special cl:special))
  (%remove-declaim-and-unquote
   (macroexpand `(cl-annot-revisit:special ,name-or-names))))

(defannotation compat-type (typespec name-or-names)
    (:inline t :alias (cl-annot-revisit:type cl:type))
  (%remove-declaim-and-unquote
   (macroexpand `(cl-annot-revisit:type ,typespec ,name-or-names))))

(defannotation compat-ftype (typespec name-or-names)
    (:inline t :alias (cl-annot-revisit:ftype cl:ftype))
  (%remove-declaim-and-unquote
   (macroexpand `(cl-annot-revisit:ftype ,typespec ,name-or-names))))

(defannotation compat-inline (name-or-names)
    (:inline t :alias (cl-annot-revisit:inline cl:inline))
  (%remove-declaim-and-unquote
   (macroexpand `(cl-annot-revisit:inline ,name-or-names))))

(defannotation compat-notinline (name-or-names)
    (:inline t :alias (cl-annot-revisit:notinline cl:notinline))
  (%remove-declaim-and-unquote
   (macroexpand `(cl-annot-revisit:notinline ,name-or-names))))

(defannotation compat-optimize (qualities)
    (:inline t :alias (cl-annot-revisit:optimize cl:optimize))
  (%remove-declaim-and-unquote
   (macroexpand `(cl-annot-revisit:optimize ,qualities))))

;;; 'declaration' was :inline in cl-annot and expanded like '(declare (declaration ...))'
;;; I think it is a bug because `cl:declaration' is proclamation only.
