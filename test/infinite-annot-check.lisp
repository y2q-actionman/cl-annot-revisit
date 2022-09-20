(in-package :cl-annot-revisit-test)

(test test-at-syntax-sharp-at-infinite-toplevel
  (is (symbol-exported-p '#:foo :cl-annot-revisit-test--infinite-annot-test))
  (is (symbol-exported-p '#:*bar* :cl-annot-revisit-test--infinite-annot-test))
  (is (symbol-exported-p '#:+baz+ :cl-annot-revisit-test--infinite-annot-test)))
