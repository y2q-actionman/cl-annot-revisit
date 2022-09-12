(defsystem #:cl-annot-revisit
  :description "Re-implementation of 'cl-annot', authored by Tomohiro Matsuyama."
  :license "WTFPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:alexandria #:named-readtables)
  :serial t
  :components ((:file "package")
               (:module "at-macro"
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
                :serial t        
                :components
                ((:file "package")
                 (:file "defmethods")
                 (:file "reader"))))
  :in-order-to ((test-op (test-op #:cl-annot-revisit-test))))
