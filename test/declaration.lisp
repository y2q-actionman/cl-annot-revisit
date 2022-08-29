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

;;; TODO: I should compare '(locally ...)' with '(let () ...)' to T.
(defun remove-let-nil (form)
  "Allegro inserts weird (let () ...) for every expansion by
  `macroexpand-all'.  I replace them to `locally' here."
  (let (head)
    (cond
      ((starts-with-subseq '(let ()) form)
       (setf head 'locally)
       (pop form)
       (pop form))
      (t
       (setf head (pop form))))
    (loop for i in form
          collect (typecase i
                    (cons
                     (remove-let-nil i))
                    (otherwise
                     i))
            into rest
          finally
             (return `(,head ,@rest)))))

(defun equal-after-macroexpand-all (macro-form expected-expansion)
  (let ((expansion (macroexpand-all macro-form)))
    #+allegro
    (let ((expansion2 (remove-let-nil expansion)))
      (equal expansion2 expected-expansion))
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


;;; TODO: in form-traversal.lisp
;;; - apply-at-macro


;;; TODO: in declaration.lisp
;;; - operator-body-location
;;; - operator-accept-docstring-in-body-p
;;; - operator-take-local-declaration-p
