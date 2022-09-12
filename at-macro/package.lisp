(in-package :cl-user)

(defpackage #:cl-annot-revisit/at-macro
  (:documentation "The at-macros of cl-annot-revisit.")
  (:use #:cl #:alexandria)
  (:import-from #:cl-annot-revisit
                #:*at-macro-verbose*
                #:at-macro-condition
                #:at-macro-style-warning
                #:at-macro-error)
  (:export
   ;; condition.lisp
   ;;
   ;; util.lisp
   ;;
   ;; eval-when.lisp
   ;;
   ;; form-traversal.lisp
   #:variable-definition-operator-p
   #:function-definition-operator-p
   #:find-name-to-be-defined-using-head
   #:find-name-to-be-defined
   #:allow-internal-form-macroexpand-p
   ;; declaration.lisp
   #:operator-body-location
   #:operator-accept-docstring-in-body-p
   #:valid-proclamation-identifier-p
   #:expand-add-declaration-using-head
   #:expand-add-declaration
   ;; declamation.lisp
   #:declaration-target-operator-p
   #:expand-incompleted-declamation-using-head
   #:expand-incompleted-declamation
   ;; documentation.lisp
   #:operator-doc-type
   #:expand-documentation-using-head
   #:expand-documentation
   ;; export.lisp
   #:expand-export-form-using-head
   #:expand-export-form
   ;; defclass.lisp
   #:expand-metaclass-using-head
   #:expand-metaclass
   #:expand-export-slots-using-head
   #:expand-export-slots
   #:*slot-accessor-option-names*
   #:expand-export-accessors-using-head
   #:expand-export-accessors
   ;; defstruct.lisp
   #:expand-export-constructors-using-head
   #:expand-export-constructors
   #:expand-export-structure-using-head
   #:expand-export-structure
   ;; slot.lisp
   #:at-required-precondition-error
   #:at-required-precondition-error-slot-name
   #:at-required-runtime-error
   #:at-required-runtime-error-slot-name
   #:at-required-runtime-error-initarg))
