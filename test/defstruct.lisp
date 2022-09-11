(in-package :cl-annot-revisit-test)

(test test-defstruct-declaration
  (is (equal-after-macroexpand
       '(cl-annot-revisit:inline
         #1=(defstruct struct slot1 slot2))
       '#1#))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:special
         #2=(defstruct struct slot1 slot2))
       '#2#))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:optimize (speed)
         #3=(defstruct struct slot1 slot2))
       '(locally (declare (optimize speed))
         #3#))))

(test test-defstruct-documentation
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation "doc"
         #1=(defstruct struct slot1 slot2))
       '(let ((#:obj #1#))
         (setf (documentation #:obj 'structure) "doc"
               (documentation #:obj 'type) "doc")
         #:obj)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation "doc"
         #2=(defstruct (struct) slot1 slot2))
       '(let ((#:obj #2#))
         (setf (documentation #:obj 'structure) "doc"
               (documentation #:obj 'type) "doc")
         #:obj)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:documentation "doc"
         #3=(defstruct (struct (:type list)) slot1 slot2))
       '(let ((#:obj #3#))
         (setf (documentation #:obj 'structure) "doc")
         #:obj))))

(test test-defstruct-export
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export
         #1=(defstruct struct1 slot1 slot2))
       '(progn (eval-when (:compile-toplevel :load-toplevel :execute)
                 (export '(struct1)))
         #1#)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export
         #2=(defstruct (struct2) slot1 slot2))
       '(progn (eval-when (:compile-toplevel :load-toplevel :execute)
                 (export '(struct2)))
         #2#))))

(test test-defstruct-metaclass
  (is (equal-after-macroexpand
       '(cl-annot-revisit:metaclass (meta)
         #1=(defstruct struct slot1 slot2))
       '#1#)))

(test test-defstruct-export-slots
  ;; EXPORT-SLOTS has no effects.
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export-slots
         #1=(defstruct struct slot1 slot2))
       '#1#)))

(test test-export-accessors-defstruct
  ;; XXX: I must let `*package*' because 1am does not preserve current package
  ;; when it runs `macroexpand-1' which will call `intern' in defstruct parser.
  (let ((*package* (find-package :cl-annot-revisit-test)))
    (is (equal-after-macroexpand
         '(cl-annot-revisit:export-accessors
           #1=(defstruct foo slot1 slot2))
         '(progn (eval-when (:compile-toplevel :load-toplevel :execute)
                   (export '(foo-slot1 foo-slot2)))
           #1#)))
    (is (equal-after-macroexpand
         '(cl-annot-revisit:export-accessors
           #2=(defstruct (foo1 (:conc-name foo1-conc-)) (slot1) (slot2 0)))
         '(progn (eval-when (:compile-toplevel :load-toplevel :execute)
                   (export '(foo1-conc-slot1 foo1-conc-slot2)))
           #2#)))
    (is (equal-after-macroexpand
         '(cl-annot-revisit:export-accessors
           #3=(defstruct (foo2 (:conc-name)) slot1 slot2))
         '(progn (eval-when (:compile-toplevel :load-toplevel :execute)
                   (export '(slot1 slot2)))
           #3#)))
    (is (equal-after-macroexpand
         '(cl-annot-revisit:export-accessors
           #4=(defstruct (foo3 :conc-name) s1 s2))
         '(progn (eval-when (:compile-toplevel :load-toplevel :execute)
                   (export '(s1 s2)))
           #4#)))
    (signals at-macro-style-warning
      (macroexpand
       '(cl-annot-revisit:export-accessors
         (defstruct (foo3 (:include other)) s1 s2))))))

(test test-defstruct-export-class
  ;; EXPORT-CLASS has no effects.
  (is (equal-after-macroexpand
       '(cl-annot-revisit:export-class
         #1=(defstruct struct slot1 slot2))
       '#1#)))

;;; TODO: export-constructors
;;; TODO: export-structure
