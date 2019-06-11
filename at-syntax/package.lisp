(in-package :cl-user)

(defpackage #:cl-annot-revisit/at-syntax
  (:documentation "The at-syntax reader macro of cl-annot-revisit.")
  (:use #:cl #:alexandria #:named-readtables)
  ;; FIXME
  (:use #:cl-annot-revisit/at-macro)
  (:export
   #:at-syntax-readtable
   #:define-at-syntax
   ))
