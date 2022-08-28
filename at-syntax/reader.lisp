(in-package #:cl-annot-revisit/at-syntax)

(defgeneric find-at-syntax-arity (operator)
  (:documentation "Returns at-syntax arity of OPERATOR. If this
  returns NIL, OPERATOR is considered as not for @-syntax.")
  (:method (operator)
    "Default is 1, means OPERATOR takes one argument for '@' syntax."
    (declare (ignore operator))
    1))

(defgeneric expand-at-read-time-p (operator)
  (:documentation "If this returns T, the macro named OPERATOR will be
  `macroexpand'ed at read-time.

  This feature is for supporting ':inline' feature of the original
  cl-annot, but not needed conceptually because you can use '#.'
  anytime.  So, this function is not exported by design.")
  (:method (operator)
    (declare (ignore operator))
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
  "The main reader of at-syntax. It reads the next symbol and collects arguments
 by ARITY or`find-at-syntax-arity', and creates a new form.
 If arity is :infinite, this function tries to read until next ')' or
 EOF. This weird feature allows us to effect the whole file."
  ;; Seeing whether '@' appeared alone. If so, returns '@' as a symbol.
  (let ((next-char (peek-char nil stream t :eof t)))
    (when (char= next-char #\space)
      (when *at-macro-verbose*
        (warn "Character '~C' appeared alone." at-char))
      (return-from read-at-syntax (intern at-char))))
  (let* ((operator (read stream t :eof t))
         (arity (or arity
                    (find-at-syntax-arity operator))))
    ;; If no arity is supplied, the operator is not registered as '@' syntax macro.
    (unless arity
      (typecase operator
        (symbol
         (when *at-macro-verbose*
           (warn "'~A', appeared after '~C', is not for @-syntax." operator at-char))
         (return-from read-at-syntax (intern (format nil "~C~A" at-char operator))))
        (t
         (when *at-macro-verbose*
           (warn "'~A', appeared after '~C', is not a symbol." operator at-char))
         (return-from read-at-syntax operator))))
    ;; Collect arguments
    (let* ((args (if (eq arity :infinite)
                     (read-delimited-list-no-eof #\) stream t)
                     (loop repeat arity
                        collect (read stream t :eof t))))
           (at-macro-form `(,operator ,@args)))
      ;; If the at-macro was requested read-time expansion by
      ;; `expand-at-read-time-p', the form is expaneded at read
      ;; time.
      (if (expand-at-read-time-p operator)
          (macroexpand at-macro-form)
          at-macro-form))))

(defun read-at (stream char)
  "The reader-macro function of '@' char."
  (read-at-syntax stream char nil))

(defun read-sharp-at (stream char n)
  "The reader-macro function of '#@' syntax."
  (read-at-syntax stream char n))

(defreadtable cl-annot-revisit:at-syntax-readtable
  (:merge :standard)
  (:macro-char #\@ #'read-at t)
  (:dispatch-macro-char #\# #\@ #'read-sharp-at))
