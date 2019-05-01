(defsystem :cl-annot-revisit
  :description "Re-implementation of 'cl-annot', authored by Tomohiro Matsuyama."
  :license "LLGPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :serial t
  ;; TODO
  :components ((:module "at-macro"
                :serial nil
                :components
                ((:file "package")
                 (:file "eval-when" :depends-on ("package"))))
	       (:module "at-syntax"))
  )
