(in-package :cl-annot-revisit-test)

(test test-optional-slots
  (is (equal-after-macroexpand
       '(cl-annot-revisit:optional some-form
         slot1)
       ''(slot1 :initform some-form :initarg :slot1)))
  (is (equal-after-macroexpand
       '(cl-annot-revisit:optional #1=(+ foo bar)
         (slot2))
       ''(slot2 :initform #1# :initarg :slot2)))
  (is (equal-after-macroexpand
       #3='(cl-annot-revisit:optional this-is-ignored
            (slot3 :initarg :argxxx :initform #2=(progn 1 2 3)))
       ''(slot3 :initarg :argxxx :initform #2#)))
  (signals at-macro-style-warning
    (let ((*at-macro-verbose* t))
      (macroexpand #3#)))
  (is (equal '(defclass foo ()
               (slot0
                #.(cl-annot-revisit:optional initform1 slot1)
                #.(cl-annot-revisit:optional initform2 (slot2 :initarg xxx))
                #.(cl-annot-revisit:optional initform3 (slot3 :initarg yyy :initform zzz))))
             '(defclass foo ()
               (slot0
                (slot1 :initform initform1 :initarg :slot1)
                (slot2 :initform initform2 :initarg xxx)
                (slot3 :initarg yyy :initform zzz))))))

(defun check-required-slot-expansion (form slot-name slot-initarg)
  (let* ((expansion (macroexpand form))
         (unquoted (second expansion)))
    (destructuring-bind (name &key initarg initform &allow-other-keys)
        unquoted
      (and (eql name slot-name)
           (equal initarg slot-initarg)
           (starts-with 'cerror initform)))))

(test test-required-slots
  (is (check-required-slot-expansion 
       '(cl-annot-revisit:required slot1)
       'slot1 :slot1))
  (is (check-required-slot-expansion
       '(cl-annot-revisit:required
         (slot2 :initarg :my-initarg))
       'slot2 :my-initarg))
  (signals cl-annot-revisit/at-macro:at-required-precondition-error
    (macroexpand
     '(cl-annot-revisit:required (slot1 :initform some-form :initarg :slot1)))))
