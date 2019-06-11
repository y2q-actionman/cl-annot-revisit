;;; TODO: move to another package.
(in-package #:cl-annot-revisit/at-syntax)

(defvar *cl-annot-compatibility* nil)

(defun find-non-@-symbol (at-symbol-name)
  ;; for achieving cl-annot compatibility, I should see non-@ symbols.
  (if *cl-annot-compatibility*
      (find-symbol (subseq at-symbol-name 1))))

(pushnew 'find-non-@-symbol *intern-at-macro-symbol-hook*)

(defconstant +cl-annot-core-package-name+ "CL-ANNOT.CORE")

(defun find-cl-annot-symbol (symbol-name)
  (if-let ((package (find-package +cl-annot-core-package-name+)))
    (values (find-symbol symbol-name package) package)
    (values nil nil)))

(defmethod find-at-syntax-arity :around ((symbol symbol))
  (or (if *cl-annot-compatibility*
          (get symbol (find-cl-annot-symbol "ANNOTATION-ARITY")))
      (call-next-method)))

(defmethod find-at-syntax-inline-p :around ((symbol symbol))
  (or (if *cl-annot-compatibility*
          (get symbol (find-cl-annot-symbol "ANNOTATION-INLINE-P")))
      (call-next-method)))


;; (define-at-syntax @eval-when-compile 1)
;; (define-at-syntax @eval-when-load 1)
;; (define-at-syntax @eval-when-execute 1)
;; (define-at-syntax @eval-when-always 1)

(define-at-syntax @ignore 1 :inline t)
(define-at-syntax @ignorable 1 :inline t)
(define-at-syntax @dynamic-extent 1 :inline t)

(define-at-syntax @special 1 :inline t)
(define-at-syntax @type 2 :inline t)
(define-at-syntax @ftype 2 :inline t)
(define-at-syntax @inline 1 :inline t)
(define-at-syntax @notinline 1 :inline t)
(define-at-syntax @optimize 1 :inline t)

;; (define-at-syntax @declaration 1) ; This is :inline in cl-annot, but not required on us.

(define-at-syntax @documentation 2)
(define-at-syntax @doc 2)

;; (define-at-syntax @export 1) ; This has :alias in cl-annot, but not required on us.

(define-at-syntax @metaclass 2)

;; (define-at-syntax @export-slots 1)
;; (define-at-syntax @export-accessors 1)
;; (define-at-syntax @export-class 1)
;; (define-at-syntax @export-constructors 1)
;; (define-at-syntax @export-structure 1)

(define-at-syntax @optional 2 :inline t)
(define-at-syntax @required 1 :inline t)


(defmacro defannotation (name lambda-list
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
