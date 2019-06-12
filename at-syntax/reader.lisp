(in-package #:cl-annot-revisit/at-syntax)

(defmacro with-temporal-package (() &body body)
  "Bind `*package*' to a temporal package and evaluates BODY."
  (let ((name (gensym "tmp-package")))
    `(let ((*package* (make-package ',name :use nil)))
       (unwind-protect (progn ,@body)
         (delete-package ',name)))))

(defvar *intern-at-macro-symbol-hook* nil
  "A list of functions called by `read-at-macro-symbol' for hooking interning process.")

(defun read-at-macro-symbol (stream at-char)
  "Reads an symbol, starting with AT-CHAR, from STREAM."
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

(defconstant +at-syntax-default-arity+ 1
  "The default arity of at-syntax macros.")

(defgeneric find-at-syntax-arity (symbol)
  (:documentation "Returns at-syntax arity of SYMBOL.")
  (:method ((symbol symbol))
    +at-syntax-default-arity+))

(defgeneric find-at-syntax-inline-p (symbol)
  (:documentation "If this returns T, the macro named SYMBOL will be expanded at read-time.
This function is not exported, by design.")
  (:method ((symbol symbol))
    nil))

(defun read-delimited-list-no-eof (char &optional (stream *standard-input*) recursive-p)
  "Reads until CHAR appearance or the end of stream."
  (let* ((last-char-stream (make-string-input-stream (string char)))
         ;; To suppress `end-of-file' error, I add ")" to the last.
         (tmp-stream (make-concatenated-stream stream last-char-stream))
         (ret (read-delimited-list char tmp-stream recursive-p)))
    (when (peek-char nil last-char-stream nil)
      ;; If last-char-stream still has a char, `read-delimited-list' does not reached to there.
      ;; Unread the char we read.
      (unread-char char stream))
    ret))

(defun read-at-syntax (stream at-char arity)
  "The main reader of at-syntax. It reads at-macro and its arguments
 by `find-at-syntax-arity' and returns a form.  If the at-macro was
 requested read-time expansion by `find-at-syntax-inline-p', the form
 is expaneded at read time."
  (let* ((at-symbol (read-at-macro-symbol stream at-char))
         (arity (or arity
                    (find-at-syntax-arity at-symbol)))
         (args (if (eq arity :infinite)
                   (read-delimited-list-no-eof #\) stream t)
                   (loop repeat arity
                      collect (read stream t nil t))))
         (at-macro-form `(,at-symbol ,@args)))
    (if (find-at-syntax-inline-p at-symbol)
        (macroexpand at-macro-form)
        at-macro-form)))

(defun read-at (stream char)
  "The reader-macro function of '@' char."
  (read-at-syntax stream char nil))

(defun read-sharp-at (stream char n)
  "The reader-macro function of '#@' syntax."
  (read-at-syntax stream char (or n :infinite)))

(defreadtable at-syntax-readtable
  (:merge :standard)
  (:macro-char #\@ #'read-at t)
  (:dispatch-macro-char #\# #\@ #'read-sharp-at))
