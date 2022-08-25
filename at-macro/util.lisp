(in-package #:cl-annot-revisit/at-macro)

#+ ()
(defun collect-special-forms (&optional (package :common-lisp))
  "Collects all special forms in PACKAGE, `:common-lisp' is default.
This is only for developing at-macro codes. Used at nowhere."
  (loop for i being the external-symbols of (find-package package)
     when (or (special-operator-p i) (macro-function i))
     collect i into ret
     finally (return (sort ret #'string< :key #'symbol-name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-list-with (obj test)
    "Do like `ensure-list', except this function returns (list OBJ)
when (funcall TEST OBJ) results true."
    (if (funcall test obj)
        (list obj)
        (ensure-list obj)))

  (defun split-list-at (n list)
    "Return first N elements and rests of LIST."
    (loop initially
         (when (zerop n)
           (return (values nil list))) 
       repeat n
       for i on list
       collect (car i) into head
       finally (return (values head (cdr i)))))

  (defun function-name-p (x)
    "Return true when it is a function name."
    (typecase x
      (symbol t)
      (cons (starts-with 'cl:setf x)))))

(defmacro mv-cond-let-n (n (&rest vars) &body clauses)
  "Multiple value variant of the famous 'COND-LET'. It takes multiple
values and see N-th value as a condition."
  (let* ((clause1 (first clauses))
         (clause1-cond (first clause1))
         (clause1-body (rest clause1)))
    (cond
      ((endp clauses)
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

(defmacro mv-cond-let2 ((&rest vars) &body clauses)
  "Uses `mv-cond-let-n' to see the second value."
  `(mv-cond-let-n 1 (,@vars) ,@clauses))

(defmacro macroexpand-convention ((original-form) &body body)
  "Executes BODY as like `progn' and return the result as the first
value.  As the second value, returns a boolean whether the result is
not same with ORIGINAL-FORM or not.
This macro is designed to follow the `macroexpand' convention."
  (let ((result (gensym)))
    `(let ((,result (progn ,@body)))
       (values ,result
               (not (eq ,result ,original-form))))))
