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
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export
         (defstruct struct1 slot1 slot2))
       '(struct1)))
  (is (expanded-export-name-equalp
       '(cl-annot-revisit:export
         (defstruct (struct2) slot1 slot2))
       '(struct2))))

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
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-accessors
           (defstruct foo slot1 slot2))
         '(foo-slot1 foo-slot2)))
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-accessors
           (defstruct (foo1 (:conc-name foo1-conc-)) (slot1) (slot2 0)))
         '(foo1-conc-slot1 foo1-conc-slot2)))
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-accessors
           (defstruct (foo2 (:conc-name)) slot1 slot2))
         '(slot1 slot2)))
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-accessors
           (defstruct (foo3 :conc-name) s1 s2))
         '(s1 s2)))
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

(test test-export-constructors
  (let ((*package* (find-package :cl-annot-revisit-test))) ; See `test-export-accessors-defstruct'
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-constructors
           (defstruct foo slot))
         '(make-foo)))
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-constructors
           (defstruct (foo1 (:conc-name cccccccc)) slot))
         '(make-foo1)))                 ; :conc-name has not effect
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-constructors
           (defstruct (foo (:constructor)) slot))
         '(make-foo)))
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-constructors
           (defstruct (foo :constructor) slot))
         '(make-foo)))
    (is (equal-after-macroexpand
         '(cl-annot-revisit:export-constructors
           #5=(defstruct (foo (:constructor nil)) slot))
         '#5#))
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-constructors
           (defstruct (foo :constructor (:constructor nil)) slot))
         '(make-foo)))
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-constructors
           (defstruct (foo :constructor (:constructor nil) (:constructor make!!)) slot))
         '(make-foo make!!)))
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-constructors
           (defstruct (foo (:constructor make??) (:constructor make!!)) slot))
         '(make?? make!!)))
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-constructors
           (defstruct (foo (:constructor make! (x y z))) slot))
         '(make!)))))

(test test-export-structure
  (let ((*package* (find-package :cl-annot-revisit-test))) ; See `test-export-accessors-defstruct'
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-structure
           (defstruct foo slot1 slot2))
         '(foo make-foo copy-foo foo-p foo-slot1 foo-slot2)))
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-structure
           (defstruct (foo (:constructor nil) :copier) slot))
         '(foo copy-foo foo-p foo-slot)))
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-structure
           (defstruct (foo (:constructor nil) (:copier nil)) slot))
         '(foo foo-p foo-slot)))
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-structure
           (defstruct (foo (:constructor) (:copier) (:predicate nil)) slot))
         '(foo make-foo copy-foo foo-slot)))
    ;; CLHS examples
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-structure
           (defstruct (door (:conc-name dr-)) knob-color width material))
         '(door make-door copy-door door-p
           dr-knob-color dr-width dr-material)))
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-structure
           (defstruct (binop (:type list) :named (:initial-offset 2))
             (operator '? :type symbol)
             operand-1
             operand-2))
         '(binop make-binop copy-binop binop-p
           binop-operator binop-operand-1 binop-operand-2)))
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-structure
           (defstruct (binop (:type list))
             (operator '? :type symbol)
             operand-1
             operand-2))
         '(binop make-binop copy-binop
           binop-operator binop-operand-1 binop-operand-2)))
    ;; some combinations
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-structure
           (defstruct (foo (:conc-name conc-)
                           (:constructor) (:constructor make!!)
                           (:copier copier!)
                           (:predicate predicate!))
             slot1 slot2))
         '(foo make-foo make!! copier! predicate! conc-slot1 conc-slot2)))
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-structure
           (defstruct (foo (:conc-name nil)
                           (:constructor nil)
                           (:copier nil)
                           (:predicate nil))
             slot1 slot2))
         '(foo slot1 slot2)))
    (is (expanded-export-name-equalp
         '(cl-annot-revisit:export-structure
           (defstruct (foo (:conc-name)
                           (:constructor)
                           (:copier)
                           (:predicate))
             slot1 slot2))
         '(foo make-foo copy-foo foo-p slot1 slot2)))))
