(in-package #:cl-annot-revisit/at-syntax)

(defmethod find-at-syntax-arity ((operator (eql 'cl-annot-revisit:type)))
  (declare (ignorable operator))
  2)

(defmethod find-at-syntax-arity ((operator (eql 'cl-annot-revisit:ftype)))
  (declare (ignorable operator))
  2)

(defmethod find-at-syntax-arity ((operator (eql 'cl-annot-revisit:documentation)))
  (declare (ignorable operator))
  2)

(defmethod find-at-syntax-arity ((operator (eql 'cl-annot-revisit:doc)))
  (declare (ignorable operator))
  2)

(defmethod find-at-syntax-arity ((operator (eql 'cl-annot-revisit:metaclass)))
  (declare (ignorable operator))
  2)

(defmethod find-at-syntax-arity ((operator (eql 'cl-annot-revisit:optional)))
  (declare (ignorable operator))
  2)
