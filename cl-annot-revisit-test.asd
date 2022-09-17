(defsystem #:cl-annot-revisit-test
  :description "Tests for cl-annot-revisit"
  :license "WTFPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :pathname #.(make-pathname :directory '(:relative "test"))
  :depends-on (#:cl-annot-revisit
               #:cl-annot-revisit-compat
               #:1am
               #:trivial-macroexpand-all)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "util-test" :depends-on ("util"))
               (:file "eval-when" :depends-on ("util"))
               (:file "declaration" :depends-on ("util"))
               (:file "declamation" :depends-on ("util"))
               (:file "documentation" :depends-on ("util"))
               (:file "export" :depends-on ("util"))
               (:file "defclass" :depends-on ("util"))
               (:file "defstruct" :depends-on ("util"))
               (:file "slot" :depends-on ("util"))
               (:file "reader" :depends-on ("util"))
               (:file "compat" :depends-on ("util" "reader")))
  :perform (prepare-op :before (o c)
             (set (find-symbol* '#:*tests* '#:1am) '()))
  :perform (test-op (o s)
             (symbol-call '#:1am '#:run)))
