(in-package :cl-annot-revisit/at-macro)

(defun collect-special-forms (&optional (package :common-lisp))
  "Collects all special forms in PACKAGE, `:common-lisp' is default.
This is only for developing at-macro codes. Used at nowhere."
  (loop for i being the external-symbols of (find-package package)
     when (or (special-operator-p i) (macro-function i))
     collect i into ret
     finally (return (sort ret #'string< :key #'symbol-name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-list-with (names &optional (test #'atom))
    "Do like `ensure-list' except testing atom with TEST."
    (cond ((null names) nil)
          ((funcall test names) (list names))
          (t names)))

  (defun split-list-at (n list)
    ;; FIXME: This code scans list twice.
    (let ((tail (nthcdr n list)))
      (values (ldiff list tail)
              tail))))

(defmacro mv-cond-let-n (n (&rest vars) &body clauses)
  "Multiple value variant of the famous 'COND-LET'. It takes multiple
values and see N-th value as a condition."
  (let* ((clause1 (first clauses))
         (clause1-cond (first clause1))
         (clause1-body (rest clause1)))
    (cond
      ((null clauses)
       nil)
      ((null clause1)
       (error "Using NIL in mv-cond-let is not allowed."))
      ((and (eql clause1-cond t) (null clause1-body))
       t)
      ((eql clause1-cond t)
       `(multiple-value-bind (,@vars) t ; This shadows outer bindings.
          (declare (ignorable ,@vars))
          ,@clause1-body))
      (t
       `(multiple-value-bind (,@vars) ,clause1-cond
	  (if ,(nth n vars)
              ,(if clause1-body
                   `(progn ,@clause1-body)
                   `(values ,@vars))
	      (mv-cond-let-n ,n (,@vars) ,@(rest clauses))))))))

(defmacro mv-cond-let ((&rest vars) &body clauses)
  "Uses `mv-cond-let-n' seeing the first value."
  `(mv-cond-let-n 0 (,@vars) ,@clauses))

(defmacro mv-cond-let2 ((&rest vars) &body clauses)
  "Uses `mv-cond-let-n' seeing the second value."
  `(mv-cond-let-n 1 (,@vars) ,@clauses))


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
