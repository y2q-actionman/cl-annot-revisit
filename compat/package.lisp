(in-package :cl-user)

(defpackage #:cl-annot-revisit-compat
  (:use #:cl #:alexandria)
  (:use #:cl-annot-revisit-at-syntax)
  (:export
    #:defannotation))
