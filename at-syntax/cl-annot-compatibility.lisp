(in-package #:cl-annot-revisit/cl-annot-interface)

(defvar *cl-annot-compatibility* nil)

(defun find-non-@-symbol (at-symbol-name)
  ;; for achieving cl-annot compatibility, I should see non-@ symbols.
  (if *cl-annot-compatibility*
      (find-symbol (subseq at-symbol-name 1))))

(pushnew 'find-non-@-symbol *intern-at-macro-symbol-hook*)

(define-constant +cl-annot-core-package-name+ "CL-ANNOT.CORE"
  :test 'equal)

(defun find-cl-annot-symbol (symbol-name)
  (if-let ((package (find-package +cl-annot-core-package-name+)))
    (values (find-symbol symbol-name package) package)
    (values nil nil)))

(defmethod find-at-syntax-arity :around ((symbol symbol))
  (or (if *cl-annot-compatibility*
          (get symbol (find-cl-annot-symbol "ANNOTATION-ARITY")))
      (call-next-method)))

(defmethod cl-annot-revisit::find-at-syntax-inline-p :around ((symbol symbol))
  (or (if *cl-annot-compatibility*
          (get symbol (find-cl-annot-symbol "ANNOTATION-INLINE-P")))
      (call-next-method)))

;;; TODO: use this and https://github.com/Shinmera/trivial-arguments
;;; for calc arity in `find-at-syntax-arity'.
(defun lambda-list-required-arguments (lambda-list)
  ;; Drop &whole or &environment and its argument.
  (loop for i = (first lambda-list)
     while (member i '(&whole &environment))
     do (setf lambda-list (nthcdr 2 lambda-list)))
  (loop for i in lambda-list
     until (member i lambda-list-keywords)
     collect i))

(defmacro defannotation
    (name lambda-list
     (&key (arity (length (lambda-list-required-arguments lambda-list)))
           (inline nil inline-supplied-p)
           alias)
     &body body)
  (let ((alias-form
         (when alias
           (when *at-macro-verbose*
             (warn ":alias keyword is not recommended."))
           `(defannotation ,alias (&rest forms) (:arity ,arity :inline ,inline)
              `(,',name ,@forms)))))
    `(progn
       (define-at-syntax ,name ,arity ,@(if inline-supplied-p
                                            `(:inline ,inline)))
       ,@(if alias-form
             `(,alias-form))
       (defmacro ,name ,lambda-list
         ,@body))))
