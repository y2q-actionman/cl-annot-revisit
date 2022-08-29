(in-package #:cl-user)

(defpackage #:cl-annot-revisit-test
  (:use #:cl #:1am #:alexandria
        #:trivial-macroexpand-all)
  (:import-from #:cl-annot-revisit
                #:*at-macro-verbose*
                #:at-macro-condition
                #:at-macro-style-warning
                #:at-macro-error))
