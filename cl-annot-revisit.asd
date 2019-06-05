(defsystem :cl-annot-revisit
  :description "Re-implementation of 'cl-annot', authored by Tomohiro Matsuyama."
  :license "WTFPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:alexandria)
  :serial t
  :components ((:module "at-macro"
                :components
                ((:file "package")
                 (:file "condition" :depends-on ("package"))
                 (:file "util" :depends-on ("package"))
                 (:file "eval-when" :depends-on ("package"))
                 (:file "form-traversal" :depends-on ("package"))
                 (:file "declaration" :depends-on ("util" "form-traversal"))
                 (:file "declamation" :depends-on ("declaration"))
                 (:file "documentation" :depends-on ("util" "form-traversal"))
                 (:file "export" :depends-on ("util" "form-traversal"))
                 (:file "defclass" :depends-on ("export"))
                 (:file "defstruct" :depends-on ("defclass"
                                                 "documentation"))
                 (:file "slot" :depends-on ("package"))))
	       (:module "at-syntax"
                :serial nil
                :components
                ((:file "package"))))
  :in-order-to ((test-op (test-op #:cl-annot-revisit-test))))
