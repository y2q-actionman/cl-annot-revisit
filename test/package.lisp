(in-package #:cl-user)

(defpackage #:cl-annot-revisit-test
  (:use #:cl #:1am #:alexandria
        #:named-readtables
        #:trivial-macroexpand-all)
  (:import-from #:cl-annot-revisit
                #:*at-macro-verbose*
                #:at-macro-condition
                #:at-macro-style-warning
                #:at-macro-error)
  (:import-from #:cl-annot-revisit-compat
                #:*cl-annot-compatibility*))
