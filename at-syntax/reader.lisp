(in-package #:cl-annot-revisit/at-syntax)

(defvar *cl-annot-compatibility* nil)

(defmacro with-temporal-package (() &body body)
  (let ((name (gensym "tmp-package")))
    `(let ((*package* (make-package ',name :use nil)))
       (unwind-protect (progn ,@body)
         (delete-package ',name)))))

(defun read-at-macro-symbol (stream at-char)
  (unread-char at-char stream)          ; push back '@' char.
  (let* ((at-symbol-tmp
          (with-temporal-package ()
            (read-preserving-whitespace stream t nil t)))
         (at-symbol-name (symbol-name at-symbol-tmp)))
    (when (and *at-macro-verbose*
               (string= at-symbol-name "@"))
      (warn "Character '@' appeared alone."))
    (or (find-symbol at-symbol-name)
        (and *cl-annot-compatibility*
             ;; for achieving cl-annot compatibility, I should see non-@ symbols.
             (find-symbol (subseq at-symbol-name 1)))
        (intern at-symbol-name))))

(defconstant +cl-annot-core-package-name+ "CL-ANNOT.CORE")

(defun find-cl-annot-symbol (symbol-name)
  (if-let ((package (find-package +cl-annot-core-package-name+)))
    (values (find-symbol symbol-name package) package)
    (values nil nil)))

(defconstant +at-syntax-default-arity+ 1)

(defgeneric find-at-syntax-arity (symbol)
  (:method ((symbol symbol))
    (or (if *cl-annot-compatibility*
            (get symbol (find-cl-annot-symbol "ANNOTATION-ARITY")))
        +at-syntax-default-arity+)))

(defgeneric find-at-syntax-inline-p (symbol)
  (:method ((symbol symbol))
    (or (if *cl-annot-compatibility*
            (get symbol (find-cl-annot-symbol "ANNOTATION-INLINE-P")))
        nil)))

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

(defreadtable at-macro-readtable
  (:merge :standard)
  (:macro-char #\@ #'read-at t)
  (:dispatch-macro-char #\# #\@ #'read-sharp-at))
