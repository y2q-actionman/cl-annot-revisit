(in-package :cl-user)

(defpackage #:cl-annot-revisit/at-syntax
  (:documentation "The at-syntax reader macro of cl-annot-revisit.")
  (:use #:cl #:cl-annot-revisit #:alexandria #:named-readtables)
  (:export
   #:*intern-at-macro-symbol-hook*
   #:at-syntax-readtable
   #:find-at-syntax-arity))
