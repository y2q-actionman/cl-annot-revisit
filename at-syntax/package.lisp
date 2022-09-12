(in-package :cl-user)

(defpackage #:cl-annot-revisit/at-syntax
  (:documentation "The at-syntax reader macro of cl-annot-revisit.")
  (:use #:cl #:alexandria #:named-readtables)
  (:import-from #:cl-annot-revisit
                #:*at-macro-verbose*
                #:find-at-syntax-arity)
  (:export
    #:*cl-annot-compatibility*
    #:find-at-syntax-arity
    #:eval-at-read-time-p
    #:resolve-at-syntax-alias))
