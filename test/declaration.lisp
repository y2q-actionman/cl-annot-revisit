(in-package #:cl-annot-revisit-test)

;;; `ignore'

(test test-decl-ignore-inline
  (is (equal
       '(let ((x 100)) #.(cl-annot-revisit:ignore x) 99)
       '(let ((x 100)) (declare (ignore x)) 99)))
  (is (equal
       '(let ((x 1)) #.(cl-annot-revisit:ignore (x x x)) 0)
       '(let ((x 1)) (declare (ignore x x x)) 0))))

(test test-decl-ignore-toplevel
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (x y z)
         (+ x y z))
       '(locally
         (declare (ignore x y z))
         (+ x y z)))))

(test test-decl-ignore-nil
  (is (equal
       '(let ((x 100)) #.(cl-annot-revisit:ignore ()) 99)
       '(let ((x 100)) (declare (ignore)) 99)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore ()
         (+ x y z))
       '(locally
         (declare (ignore))
         (+ x y z)))))

(test test-decl-ignore-function-name
  (is (equal
       '(let ((x 100)) #.(cl-annot-revisit:ignore (function foo)) 99)
       '(let ((x 100)) (declare (ignore (function foo))) 99)))
  (is (equal-after-macroexpand
       ;; This pattern does not works in the original cl-annot.
       '(cl-annot-revisit:ignore (function foo)
         (+ x y z))
       '#1=(locally
               (declare (ignore (function foo)))
             (+ x y z))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore ((function foo))
         (+ x y z))
       '#1#))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore ((function foo) bar (function baz))
         (+ x y z))
       '(locally
         (declare (ignore (function foo) bar (function baz)))
         (+ x y z)))))

(test test-decl-ignore-locally
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (foo)
         (locally (+ x y z)))
       '(locally
         (declare (ignore foo))
         (+ x y z))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (foo)
         (locally (declare (dynamic-extent)) (+ x y z)))
       '(locally
         (declare (ignore foo))
         (declare (dynamic-extent))
         (+ x y z)))))

(test test-decl-ignore-body-2
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (foo)
         (do-external-symbols (x)
           (incf *counter*)))
       '(do-external-symbols (x)
         (declare (ignore foo))
         (incf *counter*))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (foo)
         (with-input-from-string (x "hoge")
           (declare (ignore bar))
           (read x)))
       '(with-input-from-string (x "hoge")
         (declare (ignore foo))
         (declare (ignore bar))
         (read x)))))

(test test-decl-ignore-body-3
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (foo)
         (do (x)
             ((null x))
           (incf *counter*)))
       '(do (x)
         ((null x))
         (declare (ignore foo))
         (incf *counter*))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (foo)
         (multiple-value-bind (x y z) (values 1 2 3)
           (declare (ignorable x y z))
           (+ x y z)))
       '(multiple-value-bind (x y z) (values 1 2 3)
         (declare (ignore foo))
         (declare (ignorable x y z))
         (+ x y z)))))

(test test-decl-ignore-docstring
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (foo)
         (defun func (x)
           x))
       '(defun func (x)
         (declare (ignore foo))
         x)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (foo)
         (defun func (x)))
       '(defun func (x)
         (declare (ignore foo)))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (foo)
         (defun func ()
           "Hello, World!"))
       '(defun func ()
         (declare (ignore foo))
         "Hello, World!")))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (foo)
         (defun func ()
           "This is docstring"
           "Hello, World!"))
       '(defun func ()
         (declare (ignore foo))
         "This is docstring"
         "Hello, World!"))))

(test test-decl-ignore-defgeneric
  (signals at-macro-style-warning
    (let ((*at-macro-verbose* t))
      (macroexpand
       '#2=(cl-annot-revisit:ignore (x)
             #1=(defgeneric foo (&rest args))))))
  (is (equal-after-macroexpand
       '#2#
       '(locally (declare (ignore x))
         #1#)))
  ;; TODO: `optimize'
  )

(test test-decl-ignore-define-method-combination
  (signals at-macro-style-warning
    (let ((*at-macro-verbose* t))
      (macroexpand
       '(cl-annot-revisit:ignore (foo)
         (define-method-combination or)))))
  (signals at-macro-style-warning
    (let ((*at-macro-verbose* t))
      (macroexpand
       '(cl-annot-revisit:ignore (foo)
         (define-method-combination or :identity-with-one-argument t)))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (foo)
         (define-method-combination xxx (&optional (order :most-specific-first))
          ((around (:around))
           (primary (and) :order order :required t))
          etc etc etc))
       '(define-method-combination xxx (&optional (order :most-specific-first))
          ((around (:around))
           (primary (and) :order order :required t))
         (declare (ignore foo))
         etc etc etc))))

(test test-decl-ignore-defmethod
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (foo)
         (defmethod foo ()))
       '(defmethod foo ()
         (declare (ignore foo)))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (foo)
         (defmethod foo ()
           100))
       '(defmethod foo ()
         (declare (ignore foo))
         100)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (foo)
         (defmethod foo :before ()
           100))
       '(defmethod foo :before ()
         (declare (ignore foo))
         100)))
  (is (equal-after-macroexpand          ; multiple method-qualifiers.
       '(cl-annot-revisit:ignore (foo)
         (defmethod foo :before :after ()
           100))
       '(defmethod foo :before :after ()
         (declare (ignore foo))
         100)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (foo)
         (defmethod foo :before :after (x y z)
           "hoo"
           (declare (dynamic-extent))
           100))
       '(defmethod foo :before :after (x y z)
         (declare (ignore foo))
         (declare (dynamic-extent))
         "hoo"
         100))))

(test test-decl-ignore-defsetf
  (signals at-macro-style-warning
    (let ((*at-macro-verbose* t))
      (macroexpand
       '(cl-annot-revisit:ignore (foo)
         (defsetf access-fn update-fn)))))
  (signals at-macro-style-warning
    (let ((*at-macro-verbose* t))
      (macroexpand
       '(cl-annot-revisit:ignore (foo)
         (defsetf access-fn update-fn "documentation")))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (foo)
         (defsetf access-fn (x y z) (store)
           (progn)))
       '(defsetf access-fn (x y z) (store)
         (declare (ignore foo))
         (progn))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignore (foo)
         (defsetf access-fn () (store)
           "docstring"
           "Hello, World!"))
       '(defsetf access-fn () (store)
         (declare (ignore foo))
         "docstring"
         "Hello, World!"))))

(test test-decl-ignore-toplevel-multiforms
  (is (equal-after-macroexpand-all 
       '(cl-annot-revisit:ignore (x y z)
         (+ x y z)
         (defun hoge (x y z)
           (+ 1 2 3))
         (defmethod fuga ()
           "aaa"))
       '(progn
         (locally (declare (ignore x y z))
           (+ x y z))
         (defun hoge (x y z)
           (declare (ignore x y z))
           (+ 1 2 3))
         (defmethod fuga ()
           (declare (ignore x y z))
           "aaa")))))

;;; `ignorable'

(test test-decl-ignorable-inline
  (is (equal
       '(let ((x 100)) #.(cl-annot-revisit:ignorable x) 99)
       '(let ((x 100)) (declare (ignorable x)) 99)))
  (is (equal
       '(let ((x 1)) #.(cl-annot-revisit:ignorable (x x x)) 0)
       '(let ((x 1)) (declare (ignorable x x x)) 0))))

(test test-decl-ignorable-toplevel
  (is (equal-after-macroexpand
       '(cl-annot-revisit:ignorable (x y z)
         (+ x y z))
       '(locally
         (declare (ignorable x y z))
         (+ x y z)))))

;;; `dynamic-extent'

(test test-decl-dynamic-extent-inline
  (is (equal
       '(let ((x 100)) #.(cl-annot-revisit:dynamic-extent x) 99)
       '(let ((x 100)) (declare (dynamic-extent x)) 99)))
  (is (equal
       '(let ((x 1)) #.(cl-annot-revisit:dynamic-extent (x x x)) 0)
       '(let ((x 1)) (declare (dynamic-extent x x x)) 0))))

(test test-decl-dynamic-extent-toplevel
  (is (equal-after-macroexpand
       '(cl-annot-revisit:dynamic-extent (x y z)
         (+ x y z))
       '(locally
         (declare (dynamic-extent x y z))
         (+ x y z)))))

;;; These tests are for testing `add-declaration' and `apply-at-macro'.

(test test-decl-special-form
  ;; progn
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ignore (x y z)
         (progn 100
                200
                #1=(+ 1 2 3 4 5)))
       `(progn
          (locally (declare (ignore x y z)) 100)
          (locally (declare (ignore x y z)) 200)
          (locally (declare (ignore x y z)) #1#))))
  ;; eval-when
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ignore (x y z)
          (eval-when (:execute)
            (+ x y z)))
       `(eval-when (:execute)
          (locally (declare (ignore x y z))
            (+ x y z)))))
  ;; locally
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ignore (x y z)
         (locally (declare (ignore foo))))
       `(locally (declare (ignore x y z))
          (declare (ignore foo)))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ignore (x y z)
         (locally #2=(+ 1 2 3 4)))
       `(locally (declare (ignore x y z))
          #2#)))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ignore (x y z)
         (locally (declare (ignore baz))
           #3=(format t "Hello, World!")))
       `(locally (declare (ignore x y z))
          (declare (ignore baz))
          #3#)))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ignore (x y z)
         (locally ()))
       `(locally (declare (ignore x y z))
          ())))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ignore (x y z)
         (locally))
       `(locally (declare (ignore x y z))))))

(test test-decl-many-forms
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ignore (a b c)
         #2=(+ foo bar baz)
         #3=(list 1 2 3)
         (locally (declare (dynamic-extent))
           #4=(progn 1 2 3)))
       `(progn
          (locally (declare (ignore a b c)) #2#)
          (locally (declare (ignore a b c)) #3#)
          (locally (declare (ignore a b c))
            (declare (dynamic-extent))
            #4#)))))
