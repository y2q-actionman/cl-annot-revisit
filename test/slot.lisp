(in-package :cl-annot-revisit-test)

(test test-@optional
  (is (equal (macroexpand-1 '(@optional some-form slot1))
             ''(slot1 :initform some-form :initarg :slot1)))
  (is (equal (macroexpand '(@optional #1=(+ foo bar) (slot2)))
             ''(slot2 :initform #1# :initarg :slot2)))
  (is (equal (macroexpand '(@optional this-is-ignored
                            (slot3 :initarg :argxxx :initform #2=(progn 1 2 3))))
             ''(slot3 :initarg :argxxx :initform #2#)))
  (is (equal '(defclass foo ()
               (slot0
                #.(@optional initform1 slot1)
                #.(@optional initform2 (slot2 :initarg xxx))
                #.(@optional initform3 (slot3 :initarg yyy :initform zzz))))
             '(defclass foo ()
               (slot0
                (slot1 :initform initform1 :initarg :slot1)
                (slot2 :initform initform2 :initarg xxx)
                (slot3 :initarg yyy :initform zzz))))))

;;; TODO: @required
