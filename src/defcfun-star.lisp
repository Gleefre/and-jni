(in-package #:and-jni/defcfun*)

(defmacro defcfun* ((lisp-name foreign-name) return-type (&rest args) &optional docstring)
  (multiple-value-bind (lambda-list call-args returns-spec returns-read macro)
      (parse-args args)
    (if macro
        `(defmacro ,lisp-name (,@lambda-list)
           ,docstring
           `(cffi:with-foreign-objects (,@',returns-spec)
              (values (cffi:foreign-funcall ,',foreign-name ,@,call-args ,',return-type)
                      ,@',returns-read)))
        `(defun ,lisp-name (,@lambda-list)
           ,docstring
           (cffi:with-foreign-objects (,@returns-spec)
             (values (cffi:foreign-funcall ,foreign-name ,@call-args ,return-type)
                     ,@returns-read))))))
