(defsystem :cl-annot-revisit
  :description "Re-implementation of 'cl-annot', authored by Tomohiro Matsuyama."
  :license "LLGPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :serial t
  ;; TODO
  :components ((:module "at-macro")
	       (:module "at-syntax"))
  )
