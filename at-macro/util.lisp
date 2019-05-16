(in-package :cl-annot-revisit/at-macro)

(defun collect-special-forms (&optional (package (find-package :cl)))
  "Collects all special forms in PACKAGE, default is the CL package..
This is only for developing at-macro codes. Used at nowhere."
  (loop for i being the external-symbols of package
     when (or (special-operator-p i) (macro-function i))
     collect i into ret
     finally (return (sort ret #'string< :key #'symbol-name))))

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
  ((form :initarg :form)
   (message :initarg :message :initform nil))
  (:documentation "Signaled if some bad styles are found.")
  (:report
   (lambda (condition stream)
     (princ (slot-value condition 'message) stream))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun apply-to-all-forms (operator-head forms)
    (mapcar (lambda (form) (append operator-head (list form))) forms))

  (defgeneric apply-to-special-form-1* (at-macro-form form-head form)
    (:documentation
     "An internal function called by `apply-to-special-form-1'")
    (:method (at-macro-form form-head form)
      "Bottom case, returns nil."
      (declare (ignore at-macro-form form-head form))
      nil))

  (defmethod apply-to-special-form-1* (at-macro-form (form-head (eql 'progn)) form)
    `(progn ,@(apply-to-all-forms at-macro-form (rest form))))

  (defmethod apply-to-special-form-1* (at-macro-form (form-head (eql 'eval-when)) form)
    (destructuring-bind ((&rest situations) &body e-w-body) (rest form) 
      `(eval-when (,@situations)
         ,@(apply-to-all-forms at-macro-form e-w-body))))

  (defmethod apply-to-special-form-1* (at-macro-form (form-head (eql 'locally)) form)
    (multiple-value-bind (remaining-forms declarations)
        (parse-body (rest form))
      `(locally ,@declarations
         ,@(apply-to-all-forms at-macro-form remaining-forms))))

  (defun apply-to-special-form-1 (at-macro-form form)
    "If form is a special form (one of `progn', `eval-when', or
`locally'), Expand FORM into AT-MACRO-FORM recursively."
    (typecase form
      (cons (if-let ((expansion (apply-to-special-form-1* at-macro-form (first form) form)))
              (values expansion t)
              (values form nil)))
      (otherwise (values form nil)))))

;;; NOTE: At the top level, Common Lisp specially treats `macrolet',
;;; `symbol-macrolet', and *ALL* macro forms. But for supporting them
;;; I need a real code-walker.
