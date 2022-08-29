(in-package #:cl-annot-revisit-test)

(test test-ensure-list-with
  (with-function-aliasing ((ensure-list-with cl-annot-revisit/at-macro::ensure-list-with)) 
    (let ((test (complement #'listp)))
      (is (equal (ensure-list-with nil test)
                 nil))
      (is (equal (ensure-list-with 1 test)
                 '(1)))
      (is (equal (ensure-list-with '(1 2) test)
                 '(1 2))))
    (flet ((test-fn (x)
             (starts-with 'foo x)))
      (is (equal (ensure-list-with nil #'test-fn)
                 '()))
      (is (equal (ensure-list-with 1 #'test-fn)
                 '(1)))
      (is (equal (ensure-list-with '(hoge fuga) #'test-fn)
                 '(hoge fuga)))
      (is (equal (ensure-list-with '(foo 1 2 3) #'test-fn)
                 '((foo 1 2 3))))
      (is (equal (ensure-list-with '(bar (foo 1 2 3)) #'test-fn)
                 '(bar (foo 1 2 3)))))))

(test test-split-list-at
  (with-function-aliasing ((split-list-at cl-annot-revisit/at-macro::split-list-at))
    (is (multiple-value-bind (head tail) (split-list-at 0 '(0 1 2))
          (equal head nil)
          (equal tail '(0 1 2))))
    (is (multiple-value-bind (head tail) (split-list-at 1 '(0 1 2))
          (equal head '(0))
          (equal tail '(1 2))))
    (is (multiple-value-bind (head tail) (split-list-at 2 '(0 1 2))
          (equal head '(0 1))
          (equal tail '(2))))
    (is (multiple-value-bind (head tail) (split-list-at 3 '(0 1 2))
          (equal head '(0 1 3))
          (equal tail '())))
    (is (multiple-value-bind (head tail) (split-list-at 4 '(0 1 2))
          (equal head '(0 1 3))
          (equal tail '())))
    (is (multiple-value-bind (head tail) (split-list-at 0 '())
          (equal head nil)
          (equal tail nil)))
    (is (multiple-value-bind (head tail) (split-list-at 1 '())
          (equal head nil)
          (equal tail nil)))))

(with-function-aliasing ((apply-at-macro-to-special-form
                          cl-annot-revisit/at-macro::apply-at-macro-to-special-form)) 
  (test expand-recursive-1-progn
    (is (equal (apply-at-macro-to-special-form
                '(cl-annot-revisit:export)
                '(progn x y (+ x y)))
               '(PROGN
                 (cl-annot-revisit:EXPORT X)
                 (cl-annot-revisit:EXPORT Y)
                 (cl-annot-revisit:EXPORT (+ X Y))))))

  (test expand-recursive-1-eval-when
    (is (equal (apply-at-macro-to-special-form
                '(cl-annot-revisit:export)
                '(eval-when (:execute)
                  x y (+ x y)))
               '(EVAL-WHEN (:EXECUTE)
                 (cl-annot-revisit:EXPORT X)
                 (cl-annot-revisit:EXPORT Y)
                 (cl-annot-revisit:EXPORT (+ X Y))))))

  (test expand-recursive-1-locally
    (is (equal (apply-at-macro-to-special-form
                '(cl-annot-revisit:export)
                '(locally (declare (type fixnum x))
                  x y (+ x y)))
               '(LOCALLY (DECLARE (TYPE FIXNUM X))
                 (cl-annot-revisit:EXPORT X)
                 (cl-annot-revisit:EXPORT Y)
                 (cl-annot-revisit:EXPORT (+ X Y))))))

  (test expand-recursive-1-symbol-other
    (is (equal (apply-at-macro-to-special-form
                '(cl-annot-revisit:export)
                '#1=(foo ((x 100))
                      x y (+ x y)))
               '#1#))))
