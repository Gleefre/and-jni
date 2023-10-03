(in-package #:and-jni/wrappers)

(defun permission-name (name)
  (j:with-env ()
    (j:jstring-to-string
     (j:jfield :string ("android/Manifest$permission" name) :static))))

(defun get-methods (class)
  (j:jcall (:array (:class "java/lang/reflect/Method"))
      ("java/lang/Class" "getMethods")
      (if (stringp class)
          (j:jclass class)
          class)))
