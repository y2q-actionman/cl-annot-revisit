(in-package :cl-annot-revisit/at-macro)

(define-condition at-declaration-style-warning (at-macro-style-warning)
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *operator-body-location-alist*
    (append '((locally . 1))
            (mapcar
             (lambda (op) (cons op 2))
             '(with-hash-table-iterator with-package-iterator
               flet labals let let* macrolet prog prog* symbol-macrolet ; let-like
               do-all-symbols do-external-symbols do-symbols dolist dotimes ; dolist-like
               lambda pprint-logical-block
               with-input-from-string with-open-file with-open-stream with-output-to-string))
            (mapcar
             (lambda (op) (cons op 3))
             '(destructuring-bind
               define-compiler-macro define-setf-expander defmacro deftype defun ; defun-like
               do do*                   ; do-like
               multiple-value-bind
               with-accessors with-slots))))
  
  (defun operator-body-location (name)
    (cdr (assoc name *operator-body-location-alist*)))

  (defparameter *operators-accept-docstring-in-body*
    '(define-compiler-macro define-setf-expander defmacro deftype
      defun lambda)
    "List of operators accepts docstring in its body.")

  (defun operator-accept-docstring-in-body-p (name)
    (member name *operators-accept-docstring-in-body*))

  (defparameter *operators-take-local-declaration*
    '(flet labels macrolet handler-case restart-case)
    "List of operators may take local declarations.")

  (defun operator-take-local-declaration-p (name)
    (member name *operators-take-local-declaration*))

  
  (defun insert-declaration-to-body (form-body decl-specifier &key documentation whole)
    (multiple-value-bind (body decls doc)
        (parse-body form-body :documentation documentation :whole whole)
      `((declare ,decl-specifier)
        ,@decls
        ,@(if doc `(,doc)) 
        ,@body)))

  (defun insert-declaration-to-nth-body (body-index form decl-specifier &key documentation whole)
    (let* ((body (nthcdr body-index form))
           (head (ldiff form body)))
      `(,@head
        ,@(insert-declaration-to-body body decl-specifier
                                      :documentation documentation :whole whole))))


  (defgeneric insert-declaration-1* (form-head form decl-specifier)
    (:documentation "Called by `insert-declaration-1' to insert DECL-SPECIFIER into FORM.
If FORM can be expanded, returns its expansion. If not, returns nil.")
    (:method (form-head form decl-specifier)
      "The bottom case, returns nil."
      (declare (ignore form-head form decl-specifier))
      nil))

  (defmethod insert-declaration-1* ((form-head symbol) form decl-specifier)
    "General case."
    (when (operator-take-local-declaration-p form-head)
      (warn 'at-declaration-style-warning
            :message (format nil "Adding declarations into ~A form does not works for local declarations"
                             form-head) 
            :form form))
    (if-let ((body-location (operator-body-location form-head)))
      (insert-declaration-to-nth-body body-location form decl-specifier
                                      :documentation (operator-accept-docstring-in-body-p form-head)
                                      :whole form)))

  (defmethod insert-declaration-1* ((form-head (eql 'defgeneric)) form decl-specifier)
    (destructuring-bind (op function-name gf-lambda-list &rest option)
        form
      `(,op ,function-name ,gf-lambda-list (declare ,decl-specifier) ,@option)))

  (defmethod insert-declaration-1* ((form-head (eql 'define-method-combination)) form decl-specifier)
    (if (<= (length form) 3)
        (warn 'at-declaration-style-warning
              :message "The short-form of `define-method-combination' doesn't take declarations."
              :form form)
        (destructuring-bind (op name lambda-list (&rest method-group-specifier) &rest rest)
            form
          (let (options)
            (when (starts-with :arguments (first rest))
              (push (pop rest) options))
            (when (starts-with :generic-function (first rest))
              (push (pop rest) options))
            (nreversef options)
            `(,op ,name ,lambda-list (,@method-group-specifier)
                  ,@options
                  ,@(insert-declaration-to-body rest decl-specifier
                                                :whole form :documentation t))))))

  (defmethod insert-declaration-1* ((form-head (eql 'defmethod)) form decl-specifier)
    (destructuring-bind (op name &rest rest) form
      (let* ((method-qualifier (if (not (listp (first rest)))
                                   (pop rest)))
             (lambda-list (pop rest)))
        `(,op ,name ,@(if method-qualifier `(,method-qualifier)) ,lambda-list
              ,@(insert-declaration-to-body rest decl-specifier
                                            :whole form :documentation t)))))

  (defmethod insert-declaration-1* ((form-head (eql 'defsetf)) form decl-specifier)
    (if (or (<= (length form) 3)
            (stringp (fourth form)))
        (warn 'at-declaration-style-warning
              :message "The short-form of `defsetf' does not take declarations."
              :form form)
        (insert-declaration-to-nth-body 4 form decl-specifier :whole form :documentation t)))

  ;; They are easy, but are they meaningful?
  ;;   (@inline (func-a) (declaim)) ; => (declaim (inline func-a))
  (defmethod insert-declaration-1* ((form-head (eql 'declaim)) form decl-specifier)
    `(,form-head ,decl-specifier ,@(rest form)))

  (defmethod insert-declaration-1* ((form-head (eql 'proclaim)) form decl-specifier)
    `(,form-head ,decl-specifier ,@(rest form)))


  (defun insert-declaration-1 (form decl-specifier)
    "Insert DECL-SPECIFIER into FORM.
If expansion successed, returns (values <expansion> t).
If failed, returns (values <original-form> nil)."
    (typecase form
      (cons
       (if-let ((expansion (insert-declaration-1* (first form) form decl-specifier)))
         (values expansion t)
         (values form nil)))
      (otherwise (values form nil)))))


(defmacro @add-declaration (decl-specifier &body body &environment env)
  "Used by at-macros of declarations for processing recursive expansion.
If BODY is a form accepts declarations, adds a DECL-SPECIFIER into it.
If not, wraps BODY with `locally' containing DECL-SPECIFIER in it."
  (let ((at-macro-form `(@add-declaration-internal ,decl-specifier)))
    (cond
      ((not body)
       nil)
      ((not (length= 1 body))           ; recursive expansion
       `(progn ,@(apply-to-all-forms at-macro-form body)))
      (t
       (let ((form (first body)))
         (mv-cond-let2 (expansion expanded-p)
           ((insert-declaration-1 form decl-specifier)) ; try known expansions.
           ((apply-to-special-form-1 at-macro-form form)) ; try recursive expansion.
           ((macroexpand-1 form env) ; try `macroexpand-1'.
            `(,@at-macro-form ,expansion))
           (t
            `(locally (declare ,decl-specifier)
               ,form))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-at-declaration (new-decl add-declaim-p body)
    "If BODY is a form accepts declarations, adds a declaration NEW-DECL into it.
If BODY is nil, it is expanded to '(declare NEW-DECL), this is intended to embed it as a declaration using '#.'"
    (cond
      (body
       `(@add-declaration-internal ,new-decl ,@body))
      (add-declaim-p
       `(progn (declaim ,new-decl)
               '(declare ,new-decl)))   ; For '#.' combination.
      (t
       `'(declare ,new-decl)))))        ; For '#.' combination.

;;; Declaration only -- `ignore', `ignorable', `dynamic-extent'

(defmacro @ignore (variables &body body)
  "Adds `ignore' declaration into BODY.
If BODY is nil, it is expanded to '(declare (ignore ...)), this is intended to embed it as a declaration using '#.'"
  (expand-at-declaration `(ignore ,@(ensure-list variables)) nil body))

(defmacro @ignorable (variables &body body)
  "Adds `ignorable' declaration into BODY.
If BODY is nil, it is expanded to '(declare (ignorable ...)), this is intended to embed it as a declaration using '#.'"
  (expand-at-declaration `(ignorable ,@(ensure-list variables)) nil body))

(defmacro @dynamic-extent (variables &body body)
  "Adds `dynamic-extent' declaration into BODY.
If BODY is nil, it is expanded to '(declare (dynamic-extent ...)), this is intended to embed it as a declaration using '#.'"
  (expand-at-declaration `(dynamic-extent ,@(ensure-list variables)) nil body))

;;; Declaration and proclamation -- `type', `ftype', `inline', `notinline', `optimize', `special'

(defmacro @special (variables &body body)
  "Adds `special' declaration into BODY.
If BODY is nil, it is expanded to `declaim' and '(declare (special ...)), this is intended to embed it as a declaration using '#.'"
  (expand-at-declaration `(special ,@(ensure-list variables)) t body))

(defmacro @optimize (qualities &body body)
  "Adds `optimize' declaration into BODY.
If BODY is nil, it is expanded to `declaim' and '(declare (optimize ...)), this is intended to embed it as a declaration using '#.'"
  (expand-at-declaration `(qualities ,@(ensure-list qualities)) t body))



;;; TODO: `type', `ftype', `inline', `notinline'


;;; Proclamation only -- `declaration'.

(defmacro @declaration ((&rest names))
  "Just a shorthand of (declaim (declaration ...))"
  `(declaim (declaration ,@names)))




;;; FIXME: what is '@type' and `the'?
#|

at decl の locally 展開は要らない？
　関数呼び出しなら当然いらない
　special form は個別に拾っている…はず。if, multiple-value-call,progv にやってもいい気もするが、余計なお世話か
…と思ったが、internalなformのことを忘れていた。やはり必要そう。下と同じ。

両方使えるものは、トップレベルでみて自明じゃなければdeclaimとも思うが、@のネストをいい感じにしたいと考えると真面目に扱うべき。
トップレベルで見えたform全てにapplyされれば良いこととする。 apply 数を数える？
展開して複数箇所に効くようになった場合、も考慮。

単純？
　展開に成功したら t に設定する変数を用意しとく。
　各formをmacroexpand-allし、一つでも上の変数がtにならなければ、一番最初にdeclaimする。
　locallyでも囲う。
　toplevel progn だけは数の不整合が気になる。展開したげるか。

@type は、 the との関係が不明瞭

declaimについて。inline用の規約と同様、引数なしなら declaim展開でいい気がしてきた。
他はlocallyをいつも。
|#
