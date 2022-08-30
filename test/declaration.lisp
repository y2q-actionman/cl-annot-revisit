(in-package #:cl-annot-revisit-test)

(test test-decl-ignore
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
       (equal form1 form2))
      ((and (starts-with 'locally form1)
            (starts-with-subseq '(let ()) form2))
       (equal-rest-forms (cdr form1) (cddr form2)))
      ((and (starts-with-subseq '(let ()) form1)
            (starts-with 'locally form2))
       (equal-rest-forms (cddr form1) (cdr form2)))
      (t
       (equal-rest-forms form1 form2)))))

(defun equal-after-macroexpand-all (macro-form expected-expansion)
  (let ((expansion (macroexpand-all macro-form)))
    #+allegro
    (equal-ignoring-locally expansion expected-expansion)
    #-(or allegro)
    (equal expansion expected-expansion)))

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

;;; TODO: in declaration.lisp
;;; - operator-body-location
;;; - operator-accept-docstring-in-body-p
