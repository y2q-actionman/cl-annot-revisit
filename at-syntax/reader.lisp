(in-package #:cl-annot-revisit/at-syntax)

(defun whitespace-p (char)
  (declare (optimize (speed 3)))
  (declare (type character char))
  (case char
    ((#\Tab #\Newline #\Linefeed #\Page #\Return #\space) t) ; this sees the standard syntax.
    (otherwise nil)))

(defun read-sharp-at (stream char infix-arity)
  (check-type infix-arity (or null integer))
  (unread-char char stream)             ; push back '@' char.
  (let ((at-symbol (read stream t nil t)))
    ;; TODO: for achieving cl-annot compatibility, I should see non-@ symbols.
    (check-type at-symbol symbol)
    (when (and *at-macro-verbose*
               (string= (symbol-name at-symbol) "@"))
      (warn 'at-macro-style-warning :form nil
            :message "Character '@' appeared alone."))
    (let* ((arity (or infix-arity
                      (find-at-syntax-arity at-symbol)))
           (args (if (eq arity +at-syntax-inifinite-arity+)
                     (loop for form = (read stream nil 'eof t)
                        until (eq form 'eof)
                        collect form)
                     (loop repeat arity
                        collect (read stream t nil t))))
           (at-macro-form `(,at-symbol ,@args)))
      (if (find-at-syntax-inline-p at-symbol)
          (macroexpand at-macro-form)
          at-macro-form))))

(defun read-at (stream char)
  (read-sharp-at stream char nil))

(defreadtable at-macro-readtable
  (:merge :standard)
  (:macro-char #\@ #'read-at t)
  (:dispatch-macro-char #\# #\@ #'read-sharp-at))
