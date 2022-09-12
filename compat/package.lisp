(in-package :cl-user)

(defpackage #:cl-annot-revisit-compat
  (:use #:cl #:alexandria)
  (:import-from #:cl-annot-revisit
                #:*at-macro-verbose*)
  (:use #:cl-annot-revisit/at-syntax)
  (:export
    #:*cl-annot-compatibility*
    #:defannotation))
