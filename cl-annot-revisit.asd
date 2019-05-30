(defsystem :cl-annot-revisit
  :description "Re-implementation of 'cl-annot', authored by Tomohiro Matsuyama."
  :license "LLGPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:alexandria)
  :serial t
  ;; TODO
  :components ((:module "at-macro"
                :serial t               ; FIXME
                :components
                ((:file "package")
                 (:file "util" :depends-on ("package"))
                 (:file "eval-when" :depends-on ("package"))
                 (:file "export" :depends-on ("package" "util" "eval-when"))
                 (:file "declaration" :depends-on ("package" "util" "export"))
                 (:file "documentation" :depends-on ("package" "util"))
                 (:file "defclass" :depends-on ("package" "eval-when"))
                 (:file "defstruct" :depends-on ("package" "defclass"))))
	       (:module "at-syntax"
                :serial nil
                :components
                ((:file "package"))))
  :in-order-to ((test-op (test-op #:cl-annot-revisit-test))))
