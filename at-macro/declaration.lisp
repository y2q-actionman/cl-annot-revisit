(in-package :cl-annot-revisit/at-macro)

;;; Proclamation only

(defmacro @declaration (&rest names)
  "Just a shorthand of (declaim (declaration ...))"
  `(declaim (declaration ,@names)))

;;; Declaration only

(defmacro @ignore (variables &body body)
  "If BODY is a form accepts declarations, adds `ignore' declaration into it.
If BODY is nil, it is expanded to (declare (ignore ...)), this is intended to embed as a declaration using '#.'"
  (cond
    ((not body)
     `'(declare (ignore ,@(ensure-list variables))))
    (t
     (error "under implmenetation.."))))
