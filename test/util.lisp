(in-package #:cl-annot-revisit-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +at-macro-eval-always+
      '(:compile-toplevel :load-toplevel :execute)
    :test 'equal))

(defmacro mv-equal (form1 form2)
  `(equal (multiple-value-list ,form1)
          (multiple-value-list ,form2)))

(defmacro define-alias (new-name old-name)
  `(defmacro ,new-name (&rest args)
     `(,',old-name ,@args)))

(defun equal-ignoring-gensym-test-fn (lhs rhs)
  (or (equal lhs rhs)
      (and (symbolp lhs) (null (symbol-package lhs))
           (symbolp rhs) (null (symbol-package rhs)))
      ;; For Allegro `defstruct' expansion.
      ;; 0[15]: (CL-ANNOT-REVISIT-TEST::EQUAL-IGNORING-GENSYM-TEST-FN
      ;;          #<Vector @ #x1000585e6c2> #<Vector @ #x1000585e612>)
      ;; 0[15]: returned NIL
      #+allegro
      (and (arrayp lhs)
           (arrayp rhs)
           (eq (aref lhs 0) 'EXCL::DEFSTRUCT-DESCRIPTION)
           (eq (aref rhs 0) 'EXCL::DEFSTRUCT-DESCRIPTION))
      ;; For SBCL backquote internals
      ;;       2: (CL-ANNOT-REVISIT-TEST::EQUAL-IGNORING-GENSYM-TEST-FN #S(SB-IMPL::COMMA :EXPR SB-PCL::.METHOD. :KIND 0) #S(SB-IMPL::COMMA :EXPR SB-PCL::.METHOD. :KIND 0))
      ;; 2: EQUAL-IGNORING-GENSYM-TEST-FN returned NIL
      #+sbcl
      (and (typep lhs 'sb-impl::comma)
           (typep rhs 'sb-impl::comma))))

(defun equal-ignoring-gensym (lhs rhs)
  (tree-equal lhs rhs :test #'equal-ignoring-gensym-test-fn))

(defun equal-after-macroexpand (form1 form2)
  (equal-ignoring-gensym (macroexpand form1)
                         (macroexpand form2)))

(defun equal-ignoring-locally (form1 form2)
  "Allegro inserts (let () ...) instead of (locally ...) for every
  expansion by `macroexpand-all'. This function compares FORM1 and
  FORM2 considering it"
  (flet ((equal-rest-forms (form1 form2)
           (tree-equal form1 form2 :test #'equal-ignoring-locally)))
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
    (equal-ignoring-gensym expansion1 expansion2)))

(defun expanded-export-name-equalp (cl-annot-revisit-export-form name-or-names
                                    &aux (names (ensure-list name-or-names)))
  ;; (cl-annot-revisit:export (defconstant foo 100))
  ;; ->
  ;; (progn (eval-when (:compile-toplevel :load-toplevel :execute)
  ;;          (export '(foo)))
  ;;        (defconstant foo 100))
  (equal-after-macroexpand
   cl-annot-revisit-export-form
   `(progn
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (export '(,@names)))
      ,@(rest cl-annot-revisit-export-form))))

(defun find-in-nested-progn (obj progn-form)
  (or (equal-ignoring-gensym obj progn-form)
      (and (consp progn-form)
           (starts-with 'cl:progn progn-form)
           (loop for form in (rest progn-form)
                   thereis (find-in-nested-progn obj form)))))

(defun symbol-exported-p (name package)
  (eq (nth-value 1 (find-symbol (string name) package))
      :external))
