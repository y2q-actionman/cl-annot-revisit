(in-package #:cl-annot-revisit/at-syntax)

(defvar *cl-annot-compatibility* nil)
(defconstant +cl-annot-core-package-name+ "CL-ANNOT.CORE")

(defconstant +at-syntax-default-arity+ 1)
(defconstant +at-syntax-inifinite-arity+ :infinite)

(defun find-package-and-symbol (package-name symbol-name)
  (if-let ((package (find-package package-name)))
    (values (find-symbol symbol-name package) package)
    (values nil nil)))

(defun find-at-syntax-arity (symbol)
  (or (get symbol 'arity)
      (if *cl-annot-compatibility*
          (get symbol (find-package-and-symbol +cl-annot-core-package-name+
                                               "ANNOTATION-ARITY")))
      +at-syntax-default-arity+))

(defun find-at-syntax-inline-p (symbol)
  (or (get symbol 'inline-p)
      (if *cl-annot-compatibility*      ; TODO: use `defmethod' :around ?
          (get symbol (find-package-and-symbol +cl-annot-core-package-name+
                                               "ANNOTATION-INLINE-P")))
      nil))

(defun count-macro-lambda-list-required-arguments (lambda-list)
  (loop for i = (first lambda-list)
     while (member i '(&whole &environment))
     ;; Drop &whole or &environment and its argument.
     do (setf lambda-list (nthcdr 2 lambda-list)))
  (loop for i in lambda-list
     until (member i lambda-list-keywords)
     count i))

(defmacro defannotation (name lambda-list
                         (&key (arity (count-macro-lambda-list-required-arguments lambda-list))
                               alias inline)
                         &body body)
  (check-type name symbol)
  (unless (starts-with #\@ (symbol-name name))
    (warn "Name for defannotation should be started with @"))
  (let ((alias-form
         (when alias
           ;; (warn ":alias keyword is not recommended.")
           `(defannotation ,alias (&rest forms) (:arity ,arity :inline ,inline)
              `(,',name ,@forms)))))
    `(progn
       (setf (get 'name 'arity) ,arity)
       ,@(if inline
             `((setf (get 'name 'inline-p) ,inline)))
       ,@(if alias-form
             `(,alias-form))
       (defmacro ,name ,lambda-list
         ,@body))))
