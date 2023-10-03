(defsystem "and-jni"
  :description "CFFI bindings to JNI for android."
  :version "0.0.3"
  :author "Grolter <varedif.a.s@gmail.com>"
  :license "Apache 2.0"
  :depends-on ("cffi" "alexandria")
  :pathname "src"
  :components ((:file "packages")
               (:module "utils"
                :components ((:file "define-interface-function-table")))
               (:module "jni"
                :components ((:file "low-level")
                             (:file "high-level")
                             (:file "some-wrappers")))))
