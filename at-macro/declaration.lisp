(in-package :cl-annot-revisit/at-macro)

(define-condition at-declaration-style-warning (at-macro-style-warning)
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *operator-body-location-alist*
    (append '((locally . 1))
            (mapcar
             (lambda (op) (cons op 2))
             '(do-all-symbols do-external-symbols do-symbols dolist
               dotimes flet labals lambda let let* macrolet
               pprint-logical-block prog prog* symbol-macrolet
               with-hash-table-iterator with-input-from-string
               with-open-file with-open-stream with-output-to-string
               with-package-iterator))
            (mapcar
             (lambda (op) (cons op 3))
             '(define-compiler-macro define-setf-expander defmacro
               deftype defun destructuring-bind do do*
               multiple-value-bind with-accessors with-slots)))
    "Alist of operators which can be treated by our at-macros for declaration.")
  
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


  (defgeneric insert-declaration-1* (operator declaration form decl-specifier)
    (:documentation "Called by `insert-declaration-1' to insert DECL-SPECIFIER into FORM.
If FORM can be expanded, returns its expansion. If not, returns nil.")
    (:method (operator declaration form decl-specifier)
      "The bottom case, returns nil."
      (declare (ignore operator declaration form decl-specifier))
      nil))

  (defmethod insert-declaration-1* ((operator symbol) declaration form decl-specifier)
    "General case."
    (declare (ignore declaration))
    (when (operator-take-local-declaration-p operator)
      (warn 'at-declaration-style-warning :form form
            :message (format nil "Adding declarations into ~A form does not works for local declarations"
                             operator)))
    (if-let ((body-location (operator-body-location operator)))
      (insert-declaration-to-nth-body body-location form decl-specifier
                                      :documentation (operator-accept-docstring-in-body-p operator)
                                      :whole form)))

  (defmethod insert-declaration-1* ((operator (eql 'defgeneric)) (declaration (eql 'optimize))
                                    form decl-specifier)
    "`defgeneric' accepts only `optimize' declarations."
    (destructuring-bind (op function-name gf-lambda-list &rest option)
        form
      `(,op ,function-name ,gf-lambda-list (declare ,decl-specifier) ,@option)))

  (defmethod insert-declaration-1* ((operator (eql 'define-method-combination)) declaration form decl-specifier)
    (declare (ignore declaration))
    (if (<= (length form) 3)
        (warn 'at-declaration-style-warning :form form
              :message "The short-form of `define-method-combination' doesn't take declarations.")
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

  (defmethod insert-declaration-1* ((operator (eql 'defmethod)) declaration form decl-specifier)
    (declare (ignore declaration))
    (destructuring-bind (op name &rest rest) form
      (let* ((method-qualifier (if (not (listp (first rest)))
                                   (pop rest)))
             (lambda-list (pop rest)))
        `(,op ,name ,@(if method-qualifier `(,method-qualifier)) ,lambda-list
              ,@(insert-declaration-to-body rest decl-specifier
                                            :whole form :documentation t)))))

  (defmethod insert-declaration-1* ((operator (eql 'defsetf)) declaration form decl-specifier)
    (declare (ignore declaration))
    (if (or (<= (length form) 3)
            (stringp (fourth form)))
        (warn 'at-declaration-style-warning
              :message "The short-form of `defsetf' does not take declarations."
              :form form)
        (insert-declaration-to-nth-body 4 form decl-specifier :whole form :documentation t)))

  (defmethod insert-declaration-1* (operator (declaration (eql 'type))
                                    form decl-specifier)
    (if (member operator *variable-definiton-form-list*)
        (destructuring-bind (_op typespec &rest vars) decl-specifier
          (declare (ignorable _op))
          (assert (eql _op declaration))
          (let ((var-name (find-name-to-be-defined form)))
            (unless (or (null vars)           ; (@type (defvar ...)) style.
                        (member var-name vars))
              (warn 'at-declaration-style-warning :form form
                    :message (format nil "Adding ~A for ~A, but not enumerated in ~A"
                                     declaration var-name vars)))
            `(progn (declaim (,declaration ,typespec ,var-name))
                    ,form)))
        (call-next-method)))

  ;; These three forms can be implemented one function with `*variable-definiton-form-list*',
  ;; but it may contain operators respect `special' in future...
  (defmethod insert-declaration-1* ((operator (eql 'defvar)) (declaration (eql 'special))
                                    form decl-specifier)
    form)

  (defmethod insert-declaration-1* ((operator (eql 'defparameter)) (declaration (eql 'special))
                                    form decl-specifier)
    form)

  (defmethod insert-declaration-1* ((operator (eql 'defconstant)) (declaration (eql 'special))
                                    form decl-specifier)
    form)

  ;; Supporting `declaim' and `proclaim' is easy, but are they meaningful?
  ;;   (@inline (func-a) (declaim)) ; => (declaim (inline func-a))


  (defun insert-declaration-1 (form decl-specifier)
    "Insert DECL-SPECIFIER into FORM.
If expansion successed, returns (values <expansion> t).
If failed, returns (values <original-form> nil)."
    (typecase form
      (cons
       (if-let ((expansion (insert-declaration-1* (first form) (first decl-specifier)
                                                  form decl-specifier)))
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

;;; Declaration only -- `ignore', `ignorable', `dynamic-extent'

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-local-declaration (decl-name names body)
    (let* ((names-list
            ;; Do like `ensure-list' except seeing '(function name)' syntax.
            (if (or (symbolp names)
                    (and (consp names)
                         (starts-with 'function names)))
                (list names)
                names))
           (new-decl `(,decl-name ,@names-list)))
      (if body
          `(@add-declaration-internal ,new-decl ,@body)
          `'(declare ,new-decl)))))

(defmacro @ignore (names &body body)
  "Adds `ignore' declaration into BODY.
If BODY is nil, it is expanded to '(declare (ignore ...)), this is intended to embed it as a declaration using '#.'"
  (expand-local-declaration 'ignore names body))

(defmacro @ignorable (names &body body)
  "Adds `ignorable' declaration into BODY.
If BODY is nil, it is expanded to '(declare (ignorable ...)), this is intended to embed it as a declaration using '#.'"
  (expand-local-declaration 'ignorable names body))

(defmacro @dynamic-extent (names &body body)
  "Adds `dynamic-extent' declaration into BODY.
If BODY is nil, it is expanded to '(declare (dynamic-extent ...)), this is intended to embed it as a declaration using '#.'"
  (expand-local-declaration 'dynamic-extent names body))

;;; Declaration and proclamation -- `type', `ftype', `inline', `notinline', `optimize', `special'

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-declaration-and-proclamation (new-decl body)
    "If BODY is a form accepts declarations, adds a declaration NEW-DECL into it.
If BODY is nil, it is expanded to `declaim' and '(declare NEW-DECL), this is intended to embed it as a declaration using '#.'"
    (if body
        `(@add-declaration-internal ,new-decl ,@body)
        `(progn (declaim ,new-decl)
                '(declare ,new-decl)))))

(defmacro @optimize (qualities &body body)
  "Adds `optimize' declaration into BODY.
If BODY is nil, it is expanded to `declaim' and '(declare (optimize ...)), this is intended to embed it as a declaration using '#.'"
  (let ((qualities-list
         (if (or (symbolp qualities)
                 (and (consp qualities)
                      ;; There may be implementation-dependent switch. I try to match loosely.
                      (symbolp (first qualities))
                      (every #'atom (rest qualities)))) ; seeing '(speed 3)' etc.
             (list qualities)
             qualities)))
    (expand-declaration-and-proclamation `(optimize ,@qualities-list) body)))

(defmacro @special (&optional variables &body body)
  "Adds `special' declaration into BODY.
If optional VARIABLES are specified, it is used as a list of variables in `special' declaration.
If BODY is nil, it is expanded to `declaim' and '(declare (special ...)), this is intended to embed it as a declaration using '#.'"
  (let ((variables-list
         (cond ((symbolp variables)
                (list variables))
               ((and (consp variables)
                     (every #'symbolp variables)
                     (not (member (first variables) *variable-definiton-form-list*)))
                variables)
               (t
                (push variables body)
                nil))))
    (expand-declaration-and-proclamation `(special ,@variables-list) body)))

(defmacro @type (typespec &optional variables &body body)
  ;; FIXME: merge with `@special' -- into `expand-declaration-and-proclamation'.
  (let ((variables-list
         (cond ((symbolp variables)
                (list variables))
               ((and (consp variables)
                     (every #'symbolp variables)
                     (not (member (first variables) *variable-definiton-form-list*)))
                variables)
               (t
                (push variables body)
                nil))))
    (expand-declaration-and-proclamation `(type ,typespec ,@variables-list) body)))


;;; TODO: `ftype', `inline', `notinline'


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
