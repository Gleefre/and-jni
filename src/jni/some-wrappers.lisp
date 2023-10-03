(in-package #:and-jni/wrappers)

(defun permission-name (name)
  (j:with-env ()
    (j:jstring-to-string
     (j:jfield :string ("android/Manifest$permission" name) :static))))
