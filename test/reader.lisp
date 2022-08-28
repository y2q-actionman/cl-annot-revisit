(in-package :cl-annot-revisit-test)

(named-readtables:in-readtable cl-annot-revisit:at-syntax-readtable)

(test test-simple-at-syntax
  (is 
   (equal '@cl-annot-revisit:eval-when-load (1 2 3)
          '(CL-ANNOT-REVISIT:EVAL-WHEN-LOAD (1 2 3)))))
