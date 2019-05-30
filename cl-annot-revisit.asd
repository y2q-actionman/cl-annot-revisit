(defsystem :cl-annot-revisit
  :description "Re-implementation of 'cl-annot', authored by Tomohiro Matsuyama."
  :license "LLGPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:alexandria)
  :serial t
  ;; TODO
  :components ((:module "at-macro"
                :components
                ((:file "package")
                 (:file "condition" :depends-on ("package"))
                 (:file "util" :depends-on ("package"))
                 (:file "eval-when" :depends-on ("package"))
                 (:file "form-traversal" :depends-on ("package"))
                 (:file "export" :depends-on ("util" "form-traversal"))
                 (:file "declaration" :depends-on ("util" "form-traversal"))
                 (:file "documentation" :depends-on ("util" "form-traversal"))
                 (:file "defclass" :depends-on ("util" "form-traversal"))
                 (:file "defstruct" :depends-on ("util" "form-traversal"))))
	       (:module "at-syntax"
                :serial nil
                :components
                ((:file "package"))))
  :in-order-to ((test-op (test-op #:cl-annot-revisit-test))))
