(in-package #:cl-user)

(defpackage #:cl-annot-revisit-test
  (:use #:cl #:1am #:alexandria)
  (:import-from #:cl-annot-revisit
                #:*at-macro-verbose*
                #:at-macro-condition
                #:at-macro-style-warning
                #:at-macro-error))
