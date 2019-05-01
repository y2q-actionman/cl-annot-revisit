(in-package :cl-user)

(defpackage #:cl-annot-revisit/at-macro/package
  (:nicknames #:cl-annot-revisit/at-macro)
  (:documentation "The at-macros of cl-annot-revisit.")
  (:use :cl)
  (:export
   ;; see cl-annot/lib/
   ;; see std.lisp
   #:@export
   #:@ignore
   #:@ignorable
   #:@dynamic-extend
   #:@declaration
   #:@special
   #:@type
   #:@ftype
   #:@optimize
   #:@inline
   #:@notinline
   ;; eval-when -- exports them?
   #:@eval-when-compile
   #:@eval-when-load
   #:@eval-when-execute
   #:@eval-always
   ;; doc.lisp
   #:@doc
   ;; class.lisp
   #:@metaclass
   #:@export-accessors
   #:@export-constructors
   #:@export-class
   #:@export-structure
   ;; slot.lisp
   #:@optional
   #:@required
   ;; extra
   ;; #:@defannotation
   ))
