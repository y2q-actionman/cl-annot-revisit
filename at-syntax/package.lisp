(in-package :cl-user)

(defpackage #:cl-annot-revisit/at-syntax
  (:documentation "The at-syntax reader macro of cl-annot-revisit.")
  (:use #:cl #:alexandria #:named-readtables)
  (:import-from #:cl-annot-revisit/at-macro
                ;; FIXME
                #:*at-macro-verbose*
                #:at-macro-style-warning
                #:at-macro-error)
  (:export
   #:at-macro-readtable
   #:defannotation))

;;; TODO: use https://github.com/Shinmera/trivial-arguments
