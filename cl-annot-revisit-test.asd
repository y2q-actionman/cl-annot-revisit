(defsystem :cl-annot-revisit-test
  :description "Tests for cl-annot-revisit"
  :license "LLGPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :pathname #.(make-pathname :directory '(:relative "test"))
  :depends-on (#:cl-annot-revisit #:1am)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "util-test" :depends-on ("util"))
               (:file "declaration" :depends-on ("util"))
               (:file "export" :depends-on ("util")))
  :perform (prepare-op :before (o c)
             (set (find-symbol* '#:*tests* '#:1am) '()))
  :perform (test-op (o s)
             (symbol-call '#:1am '#:run)))
