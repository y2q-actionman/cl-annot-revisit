(in-package :cl-user)

(defpackage #:cl-annot-revisit/at-macro
  (:documentation "The at-macros of cl-annot-revisit.")
  (:use :cl :alexandria)
  (:export
   #:at-macro-style-warning
   ;; see cl-annot/lib/
   ;; see std.lisp
   #:@export
   ;; they are std.lisp, but declarations are different from others
   #:@ignore
   #:@ignorable
   #:@dynamic-extent
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
   #:@documentation
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
