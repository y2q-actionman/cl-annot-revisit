(in-package :cl-user)

(defpackage #:cl-annot-revisit/at-syntax
  (:documentation "The at-syntax reader macro of cl-annot-revisit.")
  (:use #:cl #:alexandria #:named-readtables)
  (:import-from #:cl-annot-revisit
                #:*at-macro-verbose*
                #:find-at-syntax-arity)
  (:export
    #:define-at-syntax
    #:find-at-syntax-arity))
