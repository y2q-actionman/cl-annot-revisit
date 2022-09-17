(in-package #:cl-annot-revisit-at-macro)

(defmacro cl-annot-revisit:eval-when-compile (&body body)
  "Just an alias of (eval-when (:compile-toplevel) ...)"
  `(eval-when (:compile-toplevel) ,@body))

(defmacro cl-annot-revisit:eval-when-load (&body body)
  "Just an alias of (eval-when (:load-toplevel) ...)"
  `(eval-when (:load-toplevel) ,@body))

(defmacro cl-annot-revisit:eval-when-execute (&body body)
  "Just an alias of (eval-when (:execute) ...)"
  `(eval-when (:execute) ,@body))

(defmacro cl-annot-revisit:eval-always (&body body)
  "Just an alias of (eval-when (:compile-toplevel :load-toplevel :execute) ...)"
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))
