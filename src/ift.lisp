(in-package #:and-jni/ift)

(defmacro define-table ((type-name struct-name &key export) &body functions)
  `(progn
     ,(when export
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (export '(,type-name ,struct-name ,@(mapcar #'car (remove-if-not #'listp functions)))
                   ,(package-name *package*))))
     (cffi:defcstruct ,struct-name
       ,@(loop for slot in functions
               collect `(,(u:ensure-car slot) :pointer)))
     (cffi:defctype ,type-name (:pointer (:struct ,struct-name)))
     ,@(loop for fun in functions
             when (and (listp fun) (<= 3 (length fun) 4))
             collect `(define-function (,type-name ,struct-name) ,@fun))))

(defmacro define-function ((type struct) name return-type (&rest args) &optional docstring)
  (multiple-value-bind (lambda-list call-args returns-spec returns-read macro)
      (parse-args args)
    (if macro
        `(defmacro ,name (,type ,@lambda-list)
           ,docstring
           `(cffi:with-foreign-objects (,@',returns-spec)
              (values (cffi:foreign-funcall-pointer (cffi:foreign-slot-value (cffi:mem-aref ,,type ',',type)
                                                                             '(:struct ,',struct)
                                                                             ',',name)
                                                    ()
                                                    ,',type ,,type
                                                    ,@,call-args
                                                    ,',return-type)
                      ,@',returns-read)))
        `(defun ,name (,type ,@lambda-list)
           ,docstring
           (cffi:with-foreign-objects (,@returns-spec)
             (values (cffi:foreign-funcall-pointer (cffi:foreign-slot-value (cffi:mem-aref ,type ',type)
                                                                            '(:struct ,struct)
                                                                            ',name)
                                                   ()
                                                   ,type ,type
                                                   ,@call-args
                                                   ,return-type)
                     ,@returns-read))))))
