(in-package #:cl-annot-revisit/at-syntax)

(defmacro with-temporal-package (() &body body)
  (let ((name (gensym "tmp-package")))
    `(let ((*package* (make-package ',name :use nil)))
       (unwind-protect (progn ,@body)
         (delete-package ',name)))))

(defvar *intern-at-macro-symbol-hook* nil)

(defun read-at-macro-symbol (stream at-char)
  (unread-char at-char stream)          ; push back '@' char.
  (let* ((at-symbol-tmp
          (with-temporal-package ()
            (read-preserving-whitespace stream t nil t)))
         (at-symbol-name (symbol-name at-symbol-tmp)))
    (when (and *at-macro-verbose*
               (string= at-symbol-name at-char))
      (warn "Character '~C' appeared alone." at-char))
    (or (find-symbol at-symbol-name)
        (loop for hook in *intern-at-macro-symbol-hook*
           when (funcall hook at-symbol-name)
           return it)
        (intern at-symbol-name))))

(defconstant +at-syntax-default-arity+ 1)

(defgeneric find-at-syntax-arity (symbol)
  (:method ((symbol symbol))
    +at-syntax-default-arity+))

(defgeneric find-at-syntax-inline-p (symbol)
  (:method ((symbol symbol))
    nil))

(defun read-at-syntax (stream at-char arity)
  (let* ((at-symbol (read-at-macro-symbol stream at-char))
         (arity (or arity
                    (find-at-syntax-arity at-symbol)))
         (args (if (eq arity :infinite)
                   (loop for form = (read stream nil 'eof t)
                      until (eq form 'eof)
                      collect form)
                   (loop repeat arity
                      collect (read stream t nil t))))
         (at-macro-form `(,at-symbol ,@args)))
    (if (find-at-syntax-inline-p at-symbol)
        (macroexpand at-macro-form)
        at-macro-form)))

(defun read-at (stream char)
  (read-at-syntax stream char nil))

(defun read-sharp-at (stream char n)
  (read-at-syntax stream char (or n :infinite)))

(defreadtable at-syntax-readtable
  (:merge :standard)
  (:macro-char #\@ #'read-at t)
  (:dispatch-macro-char #\# #\@ #'read-sharp-at))


(defmacro define-at-syntax (name arity &key (inline nil inline-supplied-p))
  (check-type name symbol)
  (when (and *at-macro-verbose*
             (not (starts-with #\@ (symbol-name name))))
    (warn "Name for defannotation should be started with @."))
  (unless (or (integerp arity)
              (eq arity :infinite))
    ;; FIXME
    (cerror "Use value."
            ":arity must be an integer or ':infinite'."))
  (when (and *at-macro-verbose*
             inline-supplied-p)
    (warn ":inline keyword is not recommended."))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defmethod find-at-syntax-arity ((_ (eql ',name)))
       (declare (ignorable _))
       ,arity)
     ,@(if inline-supplied-p
           `((defmethod find-at-syntax-inline-p ((_ (eql ',name)))
               (declare (ignorable _))
               ,inline)))))


;;; TODO: use this and https://github.com/Shinmera/trivial-arguments for calc arity.
(defun lambda-list-required-arguments (lambda-list)
  ;; Drop &whole or &environment and its argument.
  (loop for i = (first lambda-list)
     while (member i '(&whole &environment))
     do (setf lambda-list (nthcdr 2 lambda-list)))
  (loop for i in lambda-list
     until (member i lambda-list-keywords)
     collect i))
