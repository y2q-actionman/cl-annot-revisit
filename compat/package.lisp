(in-package :cl-user)

(defpackage #:cl-annot-revisit-compat
  (:use #:cl #:alexandria)
  (:import-from #:cl-annot-revisit
                #:*at-macro-verbose*)
  (:import-from #:cl-annot-revisit/at-syntax
                #:*cl-annot-compatibility*
                #:find-at-syntax-arity)
  (:export #:defannotation))
