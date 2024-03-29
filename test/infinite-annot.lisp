(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :cl-annot-revisit-test--infinite-annot-test)
    (delete-package :cl-annot-revisit-test--infinite-annot-test)))

(defpackage :cl-annot-revisit-test--infinite-annot-test
  (:use #:cl #:named-readtables))

(in-package :cl-annot-revisit-test--infinite-annot-test)

(in-readtable cl-annot-revisit:at-syntax-readtable)

#@cl-annot-revisit:export               ; Export everything after here.

(defun foo ()
  "Hello, World!")

(defvar *bar* t)

(defconstant +baz+ 9999)
