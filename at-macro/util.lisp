(in-package :cl-annot-revisit/at-macro)

(defmacro mv-cond-let* (n (&rest vars) &body clauses)
  "This is like the famous 'COND-LET', but takes multiple values and
see N-th value as a condition."
  (let ((clause1 (first clauses)))
    (cond
      ((null clauses) nil)
      ((length= clause1 1)
       `(multiple-value-bind (,@vars) ,(first clause1)
	  (declare (ignorable ,@(rest vars)))
	  (if ,(nth n vars)
	      (values ,@vars)
	      (mv-cond-let (,@vars) ,@(rest clauses)))))
      ((eql (first clause1) t)
       `(progn ,@(rest clause1)))
      (t
       `(multiple-value-bind (,@vars) ,(first clause1)
	  (declare (ignorable ,@(rest vars)))
	  (if ,(nth n vars)
	      (progn ,@(rest clause1))
	      (mv-cond-let (,@vars) ,@(rest clauses))))))))

(defmacro mv-cond-let ((&rest vars) &body clauses)
  "Uses `mv-cond-let*' seeing the first value."
  `(mv-cond-let* 0 (,@vars) ,@clauses))

(defmacro mv-cond-let2 ((&rest vars) &body clauses)
  "Uses `mv-cond-let*' seeing the second value."
  `(mv-cond-let* 1 (,@vars) ,@clauses))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun add-to-all-heads (elem list)
    (mapcar (lambda (x) (list elem x)) list))


  (defgeneric expand-recursively-1* (at-macro-name form-head form)
    (:documentation
     "An internal function called by `expand-recursively-1'.")
    (:method (at-macro-name form-head form)
      (declare (ignore at-macro-name form-head))
      (values form nil)))

  (defmethod expand-recursively-1* (at-macro-name (form-head (eql 'progn)) form)
    (values `(progn ,@(add-to-all-heads at-macro-name (rest form)))
            t))

  (defmethod expand-recursively-1* (at-macro-name (form-head (eql 'eval-when)) form)
    (destructuring-bind ((&rest situations) &body e-w-body) (rest form) 
      (values `(eval-when (,@situations)
                 ,@(add-to-all-heads at-macro-name e-w-body))
              t)))

  (defmethod expand-recursively-1* (at-macro-name (form-head (eql 'locally)) form)
    (multiple-value-bind (remaining-forms declarations)
        (parse-body (rest form))
      (values `(locally ,@declarations
                 ,@(add-to-all-heads at-macro-name remaining-forms))
              t)))

  (defmethod expand-recursively-1* (at-macro-name (form-head (eql 'symbol-macrolet)) form)
    (destructuring-bind ((&rest bindings) &body s-m-body) (rest form)
      (multiple-value-bind (remaining-forms declarations)
          (parse-body s-m-body)
        (values `(symbol-macrolet (,@bindings)
                   ,@declarations
                   ,@(add-to-all-heads at-macro-name remaining-forms))
                t))))

;;; NOTE: At the top level, Common Lisp specially treats `macrolet'
;;; and *ALL* macro forms. But for supporting them I need a real
;;; code-walker.

  (defun expand-recursively-1 (at-macro-name form)
    "Expand AT-MACRO-NAME into FORM recursively."
    (expand-recursively-1* at-macro-name (first form) form)))
