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
           initform))))

(test test-required-slots
  (is (check-required-slot-expansion 
       '(cl-annot-revisit:required slot1)
       'slot1 :slot1))
  (is (check-required-slot-expansion
       '(cl-annot-revisit:required
         (slot2 :initarg :my-initarg))
       'slot2 :my-initarg))
  (signals cl-annot-revisit-at-macro:at-required-precondition-error
    (macroexpand
     '(cl-annot-revisit:required (slot1 :initform some-form :initarg :slot1)))))

(defclass required-slot-test-class ()
  (slot0
   #.(cl-annot-revisit:required slot1)
   #.(cl-annot-revisit:required (slot2 :initarg xxx))
   #.(cl-annot-revisit:required (slot3 :accessor slot3-accessor))))

(test test-required-slot-runtime-error
  (signals cl-annot-revisit-at-macro:at-required-runtime-error
    (make-instance 'required-slot-test-class))
  (is (typep (make-instance 'required-slot-test-class :slot1 1 'xxx 2 :slot3 3)
             'required-slot-test-class)))

(test test-required-slot-runtime-error-restart
  (is (let ((obj
              (handler-bind
                  ((cl-annot-revisit-at-macro:at-required-runtime-error
                     (lambda (e)
                       (declare (ignore e))
                       (invoke-restart 'use-value 'filling-value))))
                (make-instance 'required-slot-test-class))))
        (and (eq (slot-value obj 'slot1) 'filling-value)
             (eq (slot-value obj 'slot2) 'filling-value)
             (eq (slot3-accessor obj) 'filling-value))))
  (is (let ((obj
              (handler-bind
                  ((cl-annot-revisit-at-macro:at-required-runtime-error
                     (lambda (e)
                       (let* ((slot-name
                                (cl-annot-revisit-at-macro:at-required-runtime-error-slot-name e))
                              (value (format nil "~A-VALUE" (string-upcase (string slot-name)))))
                         (use-value value e)))))
                (make-instance 'required-slot-test-class))))
        (and (string= (slot-value obj 'slot1) "SLOT1-VALUE")
             (string= (slot-value obj 'slot2) "SLOT2-VALUE")
             (string= (slot3-accessor obj) "SLOT3-VALUE")))))
