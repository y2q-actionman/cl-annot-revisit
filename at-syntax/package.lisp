(in-package :cl-user)

(defpackage #:cl-annot-revisit/at-syntax
  (:documentation "The at-syntax reader macro of cl-annot-revisit.")
  (:use #:cl #:alexandria #:named-readtables)
  (:import-from #:cl-annot-revisit
                #:*at-macro-verbose*
                #:at-macro-style-warning
                #:find-at-syntax-arity)
  (:import-from #:cl-annot-revisit/at-macro
                #:lambda-expression-p)
  (:export
    #:*cl-annot-compatibility*
    #:count-lambda-list-required-arguments
    #:find-at-syntax-arity
    #:expand-at-read-time-p
    #:resolve-at-syntax-alias))
