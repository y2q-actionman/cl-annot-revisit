(in-package #:cl-annot-revisit-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +at-macro-eval-always+
      '(:compile-toplevel :load-toplevel :execute)
    :test 'equal))

(defmacro with-function-aliasing (bindings &body body)
  (loop for (new-name old-name) in bindings
        as args-sym = (gensym "args")
        collect `(,new-name (&rest ,args-sym)
                            (declare (dynamic-extent ,args-sym))
                            (apply #',old-name ,args-sym))
          into clauses
        finally
           (return `(flet ,clauses ,@body))))

(defun equal-ignoring-gensym (x y)
  (flet ((test-fn (x y)
           (or (equal x y)
               (and (symbolp x) (null (symbol-package x))
                    (symbolp y) (null (symbol-package y))))))
    (tree-equal x y :test #'test-fn)))

(defun equal-after-macroexpand (form1 form2)
  (equal-ignoring-gensym (macroexpand form1)
                         (macroexpand form2)))

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

(defun equal-after-macroexpand-all (form1 form2)
  (let ((expansion1 (macroexpand-all form1))
        (expansion2 (macroexpand-all form2)))
    #+allegro
    (equal-ignoring-locally expansion1 expansion2)
    #-(or allegro)
    (equal expansion1 expansion2)))
