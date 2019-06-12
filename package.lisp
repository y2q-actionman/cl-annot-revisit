(in-package :cl-user)

(defpackage #:cl-annot-revisit
  (:documentation "cl-annot-revisit root package.")
  (:use)
  (:export
   ;; at-macro
   #:*at-macro-verbose*
   #:at-macro-condition
   #:at-macro-style-warning
   #:at-macro-error
   #:@add-declaration
   #:@add-declamation

   #:@eval-when-compile
   #:@eval-when-load
   #:@eval-when-execute
   #:@eval-always
   #:@ignore
   #:@ignorable
   #:@dynamic-extent
   #:@special
   #:@type
   #:@ftype
   #:@inline
   #:@notinline
   #:@optimize
   #:@declaration
   #:@documentation
   #:@doc
   #:@export
   #:@metaclass
   #:@export-slots
   #:@export-accessors
   #:@export-class
   #:@export-constructors
   #:@export-structure
   #:@optional
   #:@required
   ;; at-syntax
   #:at-syntax-readtable
   #:find-at-syntax-arity))
