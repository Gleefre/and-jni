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

(defun print-methods (class-name &optional (stream t))
  (j:with-env ()
    (j:do-jarray (method (get-methods class-name))
      (let ((str (j:jcall :string ("java/lang/reflect/Method" "toString") method)))
        (unless (cffi:null-pointer-p str)
          (format stream "~A~%" (j:jstring-to-string str)))))))
