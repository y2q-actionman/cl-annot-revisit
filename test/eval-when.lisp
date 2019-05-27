(in-package #:cl-annot-revisit-test)

(test test-@eval-when-compile
  (is (equal (macroexpand
              '(@eval-when-compile foo))
              '(eval-when (:compile-toplevel) foo))))

(test test-@eval-when-load
  (is (equal (macroexpand
              '(@eval-when-load bar))
              '(eval-when (:load-toplevel) bar))))

(test test-@eval-when-execute
  (is (equal (macroexpand
              '(@eval-when-execute baz))
              '(eval-when (:execute) baz))))

(test test-@eval-always
  (is (equal (macroexpand
              '(@eval-always qux))
              '(eval-when (:compile-toplevel :load-toplevel :execute) qux))))
