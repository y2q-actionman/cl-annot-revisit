(in-package #:cl-annot-revisit-test)

(defun expanded-export-name-equalp (cl-annot-revisit-export-form name-or-names
                                    &aux (names (ensure-list name-or-names)))
  ;; (cl-annot-revisit:export (defconstant foo 100))
  ;; ->
  ;; (progn (eval-when (:compile-toplevel :load-toplevel :execute)
  ;;          (export '(foo)))
  ;;        (defconstant foo 100))
  (equal-after-macroexpand
   cl-annot-revisit-export-form
   `(progn
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (export '(,@names)))
      ,@(rest cl-annot-revisit-export-form))))

(test test-export-single
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (defconstant hoge 100))
       'hoge))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (defun (setf func) (val) val))
       'func))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export
         (defstruct struct1
           slot1))
       'struct1))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export
         (defstruct (struct2 (:copier cpstruct2))
           slot1 slot2))
       'struct2)))

(test test-export-other
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export
         #1=(format t "Hello, World!"))
       '#1#))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export
         #2=(lambda () (format t "Hello, World!")))
       '#2#))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export
         #3=((lambda (x y z) (+ x y z)) 1 2 3))
       '#3#)))

(test test-export-many
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export
         #1=(defconstant hoge 100)
         #2=(defconstant fuga 200))
       '(progn
         (cl-annot-revisit:export #1#)
         (cl-annot-revisit:export #2#))))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export
         #3=(defconstant hoge 100)
         #4=(defun fuga () ))
       '(progn
         (cl-annot-revisit:export #3#)
         (cl-annot-revisit:export #4#)))))


(test test-export-def-forms
  ;; variables
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (defconstant +defconstant-name+ 0))
       '+defconstant-name+))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (defparameter *defparameter-name* 1))
       '*defparameter-name*))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (defvar *defvar-name*))
       '*defvar-name*))
  ;; functions
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (defgeneric defgeneric-name ()))
       'defgeneric-name))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (defgeneric (setf setf-defgeneric-name) ()))
       'setf-defgeneric-name))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (define-compiler-macro define-compiler-macro-name ()))
       'define-compiler-macro-name))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (define-compiler-macro (setf setf-define-compiler-macro-name) ()))
       'setf-define-compiler-macro-name))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (defmethod defmethod-name :around ()))
       'defmethod-name))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (defmethod (setf setf-defmethod-name) :around ()))
       'setf-defmethod-name))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (defun defun-name ()))
       'defun-name))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (defun (setf setf-defun-name) ()))
       'setf-defun-name))
  ;; others
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (defclass defclass-name () ()))
       'defclass-name))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (define-condition define-condition-name () ()))
       'define-condition-name))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (define-method-combination define-method-combination-name))
       'define-method-combination-name))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (define-modify-macro define-modify-macro-name () func))
       'define-modify-macro-name))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (define-setf-expander define-setf-expander-name (var)))
       'define-setf-expander-name))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (define-symbol-macro define-symbol-macro-name nil))
       'define-symbol-macro-name))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (defmacro defmacro-name ()))
       'defmacro-name))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (defsetf defsetf-name foo))
       'defsetf-name))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export (deftype deftype-name ()))
       'deftype-name))
  ;; defstruct and defpackage is handled specially
  )

(test test-export-defpackage-form
  (signals at-macro-style-warning
    (let ((*at-macro-verbose* t))
      (macroexpand
       '(cl-annot-revisit:export
         (defpackage defpackage-name)))))
  (is
   (let ((*at-macro-verbose* nil))
     (equal-after-macroexpand
      '(cl-annot-revisit:export
        (defpackage defpackage-name))
      '(defpackage defpackage-name)))))
