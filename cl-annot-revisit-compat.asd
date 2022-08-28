(defsystem #:cl-annot-revisit-compat
  :description "Compatibility layer for 'cl-annot'."
  :license "WTFPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:cl-annot-revisit)
  :pathname #.(make-pathname :directory '(:relative "compat"))
  :serial t
  :components ((:file "compat")
               (:file "definitions")))
