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

(with-function-aliasing ((apply-at-macro-to-special-toplevel-form
                          cl-annot-revisit/at-macro::apply-at-macro-to-special-toplevel-form)) 
  (test expand-recursive-1-progn
    (is (equal (apply-at-macro-to-special-toplevel-form
                '(test-operator)
                '(progn x y (+ x y)))
               '(PROGN
                 (test-operator X)
                 (test-operator Y)
                 (test-operator (+ X Y))))))

  (test expand-recursive-1-eval-when
    (is (equal (apply-at-macro-to-special-toplevel-form
                '(test-operator)
                '(eval-when (:execute)
                  x y (+ x y)))
               '(EVAL-WHEN (:EXECUTE)
                 (test-operator X)
                 (test-operator Y)
                 (test-operator (+ X Y))))))

  (test expand-recursive-1-locally
    (is (equal (apply-at-macro-to-special-toplevel-form
                '(test-operator)
                '(locally (declare (type fixnum x))
                  x y (+ x y)))
               '(LOCALLY (DECLARE (TYPE FIXNUM X))
                 (test-operator X)
                 (test-operator Y)
                 (test-operator (+ X Y))))))

  (test expand-recursive-1-macrolet
    (is (equal (apply-at-macro-to-special-toplevel-form
                '(test-operator)
                '(macrolet ((hoge (x) x))
                  (declare (type fixnum x))
                  x y (+ x y)))
               '(macrolet ((hoge (x) x))
                 (declare (type fixnum x))
                 (test-operator X)
                 (test-operator Y)
                 (test-operator (+ X Y))))))

  (test expand-recursive-1-symbol-macrolet
    (is (equal (apply-at-macro-to-special-toplevel-form
                '(test-operator)
                '(symbol-macrolet ((hoge (x) x))
                  (declare (type fixnum x))
                  x y (+ x y)))
               '(symbol-macrolet ((hoge (x) x))
                 (declare (type fixnum x))
                 (test-operator X)
                 (test-operator Y)
                 (test-operator (+ X Y))))))

  (test expand-recursive-1-symbol-other
    (is (equal (apply-at-macro-to-special-toplevel-form
                '(test-operator)
                '#1=(tagbody ((x 100))
                      x y (+ x y)))
               '#1#))))

(defmacro test-macro-1 (&body body)
  `(+ (progn ,@body) 9999))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod cl-annot-revisit/at-macro:allow-internal-form-macroexpand-p ((op (eql 'test-macro-1)))
    (declare (ignorable op))
    t))

(with-function-aliasing ((apply-at-macro cl-annot-revisit/at-macro::apply-at-macro)) 
  (test apply-at-macro-zero-form
    (is (mv-equal (apply-at-macro '(never-used) (constantly nil)
                                  () nil)
                  (values nil nil))))
  
  (test apply-at-macro-multi-forms
    (is (mv-equal (apply-at-macro '(test-operator) (constantly nil)
                                  '(1 2 3 4 5) nil)
                  (values '(progn (test-operator 1)
                            (test-operator 2)
                            (test-operator 3)
                            (test-operator 4)
                            (test-operator 5))
                          t))))
  
  (test apply-at-macro-expander
    (is (mv-equal (apply-at-macro '(test-operator)
                                  (lambda (x)
                                    (values (list* :hello x) t))
                                  '((1 2 3 4 5)) nil)
                  (values '(:hello 1 2 3 4 5)
                          t))))

  (test apply-at-macro-special-form
    (is (mv-equal (apply-at-macro '(test-operator) (constantly nil)
                                  '((locally (declare) 1 2 3)) nil)
                  (values '(locally (declare)
                            (test-operator 1)
                            (test-operator 2)
                            (test-operator 3))
                          t))))

  (test apply-at-macro-macroexpand-1
    (is (mv-equal (apply-at-macro '(test-operator) (constantly nil)
                                  '((test-macro-1 () 1 2 3)) nil)
                  (values `(test-operator (+ (progn () 1 2 3) 9999))
                          t))))

  (test apply-at-macro-not-macroexpand-1
    (is (mv-equal (apply-at-macro '(test-operator) (constantly nil)
                                  '((test-macro-2 ())) nil)
                  (values '(test-macro-2 ())
                          nil))))

  (test apply-at-macro-no-expansion
    (is (mv-equal (apply-at-macro '(never-used) (constantly nil)
                                  '(#1=(list 1 2 3)) nil)
                  (values '#1#nil)))))
