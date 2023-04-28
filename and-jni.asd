(defsystem "and-jni"
  :description "CFFI bindings to JNI for android."
  :version "0.0.3"
  :author "Grolter <varedif.a.s@gmail.com>"
  :license "Apache 2.0"
  :depends-on ("cffi" "alexandria")
  :components ((:file "packages")
               (:file "define-interface-function-table")
               (:file "jni-cffi")
               (:file "jni")))
