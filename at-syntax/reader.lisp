(in-package #:cl-annot-revisit/at-syntax)

(defvar *cl-annot-compatibility* nil)

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

(defun read-at-syntax (stream at-char)
  "The reader-macro function of '@' char. It reads the next symbol and
 collects arguments by ARITY or`find-at-syntax-arity', and creates a
 new form.
 If arity is :infinite, this function tries to read until next ')' or
 EOF. This weird feature allows us to effect the whole file."
  (let* ((operator (read stream t :eof t))
         (operator (resolve-at-syntax-alias operator *cl-annot-compatibility*))
         (arity (find-at-syntax-arity operator *cl-annot-compatibility*)))
    ;; If no arity is supplied, the operator is not registered as '@' syntax macro.
    (unless arity
      (when *at-macro-verbose*
        (warn 'at-macro-style-warning
              :message (format nil "'~A', appeared after '~C', is not for @-syntax."
                               operator at-char)
              :form operator))
      (return-from read-at-syntax operator))
    ;; Collect arguments
    (let* ((args (if (eq arity :infinite)
                     (read-delimited-list-no-eof #\) stream t)
                     (loop repeat arity
                        collect (read stream t :eof t))))
           (at-macro-form
             (cond
               ((and (consp operator)
                     (not (lambda-expression-p operator)))
                `(,@operator ,@args))   ; @() support
               (t
                `(,operator ,@args)))))
      ;; If the at-macro was requested read-time eval by
      ;; `eval-at-read-time-p', the form is expaneded at read
      ;; time.
      (if (eval-at-read-time-p operator *cl-annot-compatibility*)
          (eval at-macro-form)
          at-macro-form))))

(defreadtable cl-annot-revisit:at-syntax-readtable
  (:merge :standard)
  (:macro-char #\@ #'read-at-syntax t))
