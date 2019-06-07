(in-package :cl-annot-revisit-test)

#|
CL-ANNOT-REVISIT/AT-SYNTAX> (count-macro-lambda-list-required-arguments '())
0
CL-ANNOT-REVISIT/AT-SYNTAX> (count-macro-lambda-list-required-arguments '(&whole a))
0
CL-ANNOT-REVISIT/AT-SYNTAX> (count-macro-lambda-list-required-arguments '(&whole a x y z))
3
CL-ANNOT-REVISIT/AT-SYNTAX> (count-macro-lambda-list-required-arguments '(&whole a x y z &key))
3
|#



(defannotation @hoge (a) (:alias @fuga)
  `(let ((obj ,a))
     (pprint obj)
     obj))

(@hoge 100)
(@fuga 100)
