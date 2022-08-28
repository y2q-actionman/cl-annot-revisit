(defpackage #:cl-annot-revisit-compat
  (:use #:cl #:alexandria)
  (:import-from #:cl-annot-revisit
                #:*at-macro-verbose*)
  (:import-from #:cl-annot-revisit/at-syntax
                #:find-at-syntax-arity)
  (:export #:*cl-annot-compatibility*
           #:defannotation))

(in-package #:cl-annot-revisit-compat)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *cl-annot-compatibility* nil)

  (define-constant +cl-annot-core-package-name+ "CL-ANNOT.CORE"
    :test 'equal)

  (defun find-cl-annot-symbol (symbol-name)
    (if-let ((package (find-package +cl-annot-core-package-name+)))
      (find-symbol symbol-name package)))

  (defmethod cl-annot-revisit/at-syntax:find-at-syntax-arity :around ((symbol symbol))
    (or (if *cl-annot-compatibility*
            (get symbol (find-cl-annot-symbol "ANNOTATION-ARITY")))
        (call-next-method)))

  (defmethod cl-annot-revisit/at-syntax::expand-at-read-time-p :around ((symbol symbol))
    (or (if *cl-annot-compatibility*
            (get symbol (find-cl-annot-symbol "ANNOTATION-INLINE-P")))
        (call-next-method)))

  (defun lambda-list-required-arguments (lambda-list)
    "Returns list of symbols naming required arguments in LAMBDA-LIST."
    ;; Drop &whole or &environment and its argument.
    (loop for i = (first lambda-list)
          while (member i '(&whole &environment))
          do (setf lambda-list (nthcdr 2 lambda-list)))
    (loop for i in lambda-list
          until (member i lambda-list-keywords)
          collect i))
  ;; TODO: use this and https://github.com/Shinmera/trivial-arguments
  ;; for calc arity in `find-at-syntax-arity'.
  )

(defmacro defannotation
    (name lambda-list
     (&key (arity (length (lambda-list-required-arguments lambda-list)))
        (inline nil inline-supplied-p)
        alias)
     &body body)
  "`cl-annot:defannotation' like one."
  (let ((alias-form
          (when alias
            (when *at-macro-verbose*
              (warn ":alias keyword is not recommended."))
            `(defannotation ,alias (&rest forms) (:arity ,arity :inline ,inline)
               `(,',name ,@forms)))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defmethod find-at-syntax-arity ((_ (eql ',name)))
           (declare (ignorable _))
           ,arity)
         ,@(if inline-supplied-p
               `((defmethod cl-annot-revisit/at-syntax::expand-at-read-time-p ((_ (eql ',name)))
                   (declare (ignorable _))
                   ,inline))))
       ,@(if alias-form
             `(,alias-form))
       (defmacro ,name ,lambda-list
         ,@body))))
