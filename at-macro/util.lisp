(in-package :cl-annot-revisit/at-macro)

(defmacro mv-cond-let* (n (&rest vars) &body clauses)
  "This is like the famous 'COND-LET', but takes multiple values and
see N-th value as a condition."
  (let ((clause1 (first clauses)))
    (cond
      ((null clauses) nil)
      ((length= 1 clause1)
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


(define-condition at-macro-style-warning (style-warning)
  ()
  (:documentation "Signaled if some bad styles are found."))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun add-to-all-heads (elem list)
    (mapcar (lambda (x) (list elem x)) list))

  (defgeneric apply-to-special-form-1* (at-macro-name form-head form)
    (:documentation
     "An internal function called by `apply-to-special-form-1'")
    (:method (at-macro-name form-head form)
      (declare (ignore at-macro-name form-head))
      (values form nil)))

  (defun apply-to-progn-form (at-macro-name form)
    (assert (eq (first form) 'progn))
    `(progn ,@(add-to-all-heads at-macro-name (rest form))))

  (defmethod apply-to-special-form-1* (at-macro-name (form-head (eql 'progn)) form)
    (values `(progn ,@(add-to-all-heads at-macro-name (rest form)))
            t))

  (defmethod apply-to-special-form-1* (at-macro-name (form-head (eql 'eval-when)) form)
    (destructuring-bind ((&rest situations) &body e-w-body) (rest form) 
      (values `(eval-when (,@situations)
                 ,@(add-to-all-heads at-macro-name e-w-body))
              t)))

  (defmethod apply-to-special-form-1* (at-macro-name (form-head (eql 'locally)) form)
    (multiple-value-bind (remaining-forms declarations)
        (parse-body (rest form))
      (values `(locally ,@declarations
                 ,@(add-to-all-heads at-macro-name remaining-forms))
              t)))

  ;; FIXME: Is this only for `@export'?
  (defun apply-to-special-form-1 (at-macro-name form)
    "If form is a special form (one of `progn', `eval-when', or
`locally'), Expand AT-MACRO-NAME into FORM recursively."
    (apply-to-special-form-1* at-macro-name (first form) form)))

;;; NOTE: At the top level, Common Lisp specially treats `macrolet',
;;; `symbol-macrolet', and *ALL* macro forms. But for supporting them
;;; I need a real code-walker.

#+ ()
(defun collect-special-forms ()
  (loop for i being the external-symbols of (find-package :cl)
     when (or (special-operator-p i) (macro-function i))
     collect i into specials
     finally (return (sort specials #'string< :key #'symbol-name))))
