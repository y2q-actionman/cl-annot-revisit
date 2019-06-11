(in-package :cl-user)

(defpackage #:cl-annot-revisit/at-syntax
  (:documentation "The at-syntax reader macro of cl-annot-revisit.")
  (:use #:cl #:alexandria #:named-readtables)
  (:use #:cl-annot-revisit/at-macro) ; FIXME
  (:export
   #:at-syntax-readtable
   #:find-at-syntax-arity))

(defpackage #:cl-annot-revisit/cl-annot-interface
  (:use)
  (:export #:defannotation))
