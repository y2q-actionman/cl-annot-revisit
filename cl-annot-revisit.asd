(defsystem :cl-annot-revisit
  :description "Re-implementation of 'cl-annot', authored by Tomohiro Matsuyama."
  :license "LLGPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:alexandria)
  :serial t
  ;; TODO
  :components ((:module "at-macro"
                :serial nil
                :components
                ((:file "package")
                 (:file "util" :depends-on ("package"))
                 (:file "eval-when" :depends-on ("package"))
                 (:file "declaration" :depends-on ("package" "util"))
                 (:file "export" :depends-on ("package" "util" "eval-when"))))
	       (:module "at-syntax"
                :serial nil
                :components
                ((:file "package"))))
  :in-order-to ((test-op (test-op #:cl-annot-revisit-test))))
