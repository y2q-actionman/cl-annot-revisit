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
   #:find-name-to-be-defined-using-head
   #:find-name-to-be-defined
   ;; declaration.lisp
   #:expand-add-declaration-using-head
   #:expand-add-declaration
   ;; declamation.lisp
   #:expand-add-declamation-using-head
   #:expand-add-declamation
   ;; documentation.lisp
   #:expand-documentation-using-head
   #:expand-documentation
   ;; export.lisp
   #:expand-export-form-using-head
   #:expand-export-form
   ;; defclass.lisp
   #:expand-metaclass-using-head
   #:expand-metaclass
   #:expand-export-slots-1*
   #:expand-export-slots-1
   #:*slot-accessor-option-names*
   #:expand-export-accessors-1*
   #:expand-export-accessors-1
   ;; defstruct.lisp
   #:expand-export-constructors-1*
   #:expand-export-constructors-1
   #:expand-export-structure-1*
   #:expand-export-structure-1
   ;; slot.lisp
   #:at-required-precondition-error
   #:at-required-runtime-error
   ))
