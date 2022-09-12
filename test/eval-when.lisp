(in-package #:cl-annot-revisit-test)

(test test-eval-when-compile
  (is (equal-after-macroexpand
       '(cl-annot-revisit:eval-when-compile foo)
       '(eval-when (:compile-toplevel) foo))))

(test test-eval-when-load
  (is (equal-after-macroexpand
       '(cl-annot-revisit:eval-when-load bar)
       '(eval-when (:load-toplevel) bar))))

(test test-eval-when-execute
  (is (equal-after-macroexpand
       '(cl-annot-revisit:eval-when-execute baz)
       '(eval-when (:execute) baz))))

(test test-eval-always
  (is (equal-after-macroexpand
       '(cl-annot-revisit:eval-always qux)
       '(eval-when (:compile-toplevel :load-toplevel :execute) qux))))
