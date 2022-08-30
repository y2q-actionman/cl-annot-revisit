(in-package #:cl-annot-revisit-test)

(defun equal-ignoring-locally (form1 form2)
  "Allegro inserts (let () ...) instead of (locally ...) for every
  expansion by `macroexpand-all'. This function compares FORM1 and
  FORM2 considering it"
  (flet ((equal-rest-forms (form1 form2)
           (declare (type list form1 form2))
           (loop for i-cons on form1
                 for j-cons on form2
                 always (equal-ignoring-locally (car i-cons) (car j-cons))
                 finally (return (and (null (cdr i-cons))
                                      (null (cdr j-cons)))))))
    (cond
      ((not (and (consp form1)
                 (consp form2)))
       (equal-ignoring-gensym form1 form2))
      ((and (starts-with 'locally form1)
            (starts-with-subseq '(let ()) form2))
       (equal-rest-forms (cdr form1) (cddr form2)))
      ((and (starts-with-subseq '(let ()) form1)
            (starts-with 'locally form2))
       (equal-rest-forms (cddr form1) (cdr form2)))
      (t
       (equal-rest-forms form1 form2)))))

(defun equal-after-macroexpand-all (macro-form expected-expansion)
  (let ((expansion1 (macroexpand-all macro-form))
        (expansion2 (macroexpand-all expected-expansion)))
    #+allegro
    (equal-ignoring-locally expansion1 expansion2)
    #-(or allegro)
    (equal expansion1 expansion2)))

(test test-decl-ignore-inline
  (is (equal
       '(let ((x 100)) #.(cl-annot-revisit:ignore x) 99)
       '(let ((x 100)) (declare (ignore x)) 99)))
  (is (equal
       '(let ((x 1)) #.(cl-annot-revisit:ignore (x x x)) 0)
       '(let ((x 1)) (declare (ignore x x x)) 0))))

(test test-decl-ignore-toplevel
  (is (equal
       (macroexpand
        '(cl-annot-revisit:ignore (x y z)
          (+ x y z)))
       '(locally
         (declare (ignore x y z))
         (+ x y z)))))

(test test-decl-ignore-nil
  (is (equal
       '(let ((x 100)) #.(cl-annot-revisit:ignore ()) 99)
       '(let ((x 100)) (declare (ignore)) 99)))
  (is (equal
       (macroexpand
        '(cl-annot-revisit:ignore ()
          (+ x y z)))
       '(locally
         (declare (ignore))
         (+ x y z)))))

(test test-decl-ignore-function-name
  (is (equal
       '(let ((x 100)) #.(cl-annot-revisit:ignore (function foo)) 99)
       '(let ((x 100)) (declare (ignore (function foo))) 99)))
  (is (equal
       (macroexpand
        '(cl-annot-revisit:ignore (function foo)
          (+ x y z)))
       '#1=(locally
               (declare (ignore (function foo)))
             (+ x y z))))
  (is (equal
       (macroexpand
        '(cl-annot-revisit:ignore ((function foo))
          (+ x y z)))
       '#1#))
  (is (equal
       (macroexpand
        '(cl-annot-revisit:ignore ((function foo) bar (function baz))
          (+ x y z)))
       '(locally
         (declare (ignore (function foo) bar (function baz)))
         (+ x y z)))))

(test test-decl-ignore-locally
  (is (equal
       (macroexpand
        '(cl-annot-revisit:ignore (foo)
          (locally (+ x y z))))
       '(locally
         (declare (ignore foo))
         (+ x y z))))
  (is (equal
       (macroexpand
        '(cl-annot-revisit:ignore (foo)
          (locally (declare (dynamic-extent)) (+ x y z))))
       '(locally
         (declare (ignore foo))
         (declare (dynamic-extent))
         (+ x y z)))))

(test test-decl-ignore-body-2
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ignore (foo)
          (do-external-symbols (x)
            (incf *counter*)))
       '(do-external-symbols (x)
         (declare (ignore foo))
         (incf *counter*))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ignore (foo)
         (with-input-from-string (x "hoge")
           (declare (ignore bar))
           (read x)))
       '(with-input-from-string (x "hoge")
         (declare (ignore foo))
         (declare (ignore bar))
         (read x)))))

(test test-decl-ignore-body-3
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ignore (foo)
         (do (x)
             ((null x))
           (incf *counter*)))
       '(do (x)
         ((null x))
         (declare (ignore foo))
         (incf *counter*))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ignore (foo)
         (multiple-value-bind (x y z) (values 1 2 3)
           (declare (ignorable x y z))
           (+ x y z)))
       '(multiple-value-bind (x y z) (values 1 2 3)
         (declare (ignore foo))
         (declare (ignorable x y z))
         (+ x y z)))))

(test test-decl-ignore-docstring
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ignore (foo)
         (defun func (x)
           x))
       '(defun func (x)
         (declare (ignore foo))
         x)))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ignore (foo)
         (defun func (x)))
       '(defun func (x)
         (declare (ignore foo)))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ignore (foo)
         (defun func ()
           "Hello, World!"))
       '(defun func (x)
         (declare (ignore foo))
         "Hello, World!")))
  (is (equal-after-macroexpand-all
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
       '(cl-annot-revisit:ignore (x)
         (defgeneric foo (&rest args))))))
  (is (equal-after-macroexpand-all
       '(cl-annot-revisit:ignore (x)
         (defgeneric foo (&rest args)))
       '(locally (declare (ignore foo))
         (defgeneric foo (&rest args)))))
  ;; TODO: `optimize'
  )

;;; TODO: define-method-combination, defmethod, defsetf


;;; ignorable

(test test-decl-ignorable-inline
  (is (equal
       '(let ((x 100)) #.(cl-annot-revisit:ignorable x) 99)
       '(let ((x 100)) (declare (ignorable x)) 99)))
  (is (equal
       '(let ((x 1)) #.(cl-annot-revisit:ignorable (x x x)) 0)
       '(let ((x 1)) (declare (ignorable x x x)) 0))))

(test test-decl-ignorable-toplevel
  (is (equal
       (macroexpand
        '(cl-annot-revisit:ignorable (x y z)
          (+ x y z)))
       '(locally
         (declare (ignorable x y z))
         (+ x y z)))))

;;; dynamic-extent

(test test-decl-dynamic-extent-inline
  (is (equal
       '(let ((x 100)) #.(cl-annot-revisit:dynamic-extent x) 99)
       '(let ((x 100)) (declare (dynamic-extent x)) 99)))
  (is (equal
       '(let ((x 1)) #.(cl-annot-revisit:dynamic-extent (x x x)) 0)
       '(let ((x 1)) (declare (dynamic-extent x x x)) 0))))

(test test-decl-dynamic-extent-toplevel
  (is (equal
       (macroexpand
        '(cl-annot-revisit:dynamic-extent (x y z)
          (+ x y z)))
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
