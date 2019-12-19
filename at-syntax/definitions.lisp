(in-package #:cl-annot-revisit/at-syntax)

(defmacro define-at-syntax (name arity &key ((inline inline) nil inline-supplied-p))
  "Internal helper"
  (check-type name symbol)
  (when (and *at-macro-verbose*
             (starts-with #\@ (symbol-name name)))
    (warn "Name for define-at-syntax should not be started with @."))
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

(define-at-syntax cl-annot-revisit:eval-when-compile 1)
(define-at-syntax cl-annot-revisit:eval-when-load 1)
(define-at-syntax cl-annot-revisit:eval-when-execute 1)
(define-at-syntax cl-annot-revisit:eval-always 1)

#|

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
|#
