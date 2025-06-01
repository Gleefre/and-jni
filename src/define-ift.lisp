(in-package #:and-jni/define-ift)

(defun rest-arg-p (args)
  (let ((arg (car (last args))))
    (and (eq '&rest (car arg))
         (cadr arg))))

(defun generate-lambda-list (args)
  (loop with rest = nil
        with optional = nil
        for (type argname . default-value) in args

        if rest
          do (error "Found arguments after &rest. ~S"
                    (list* type argname default-value))
        else when (eq type '&rest)
          collect '&rest and
          do (setf rest t)

        if (and default-value rest)
          do (error "Default value specified for the &rest argument. ~S"
                    (list* type argname default-value))
        else if (and default-value (not optional))
          collect '&optional and
          do (setf optional t)
        else when (and optional (not rest) (not default-value))
          do (warn "No default value found for an argument after &optional. ~S"
                   (list* type argname default-value))

        unless (eq (u:ensure-car type) :return)
          collect (if (and optional (not rest))
                      `(,argname ,@default-value)
                      argname)))

(defun generate-foreign-objects (args &optional prefix)
  (loop for (type argname) in args
        when (eq type :return)
        collect `(,@prefix ,argname :pointer)
        when (and (listp type)
                  (eq (car type) :return))
        collect `(,@prefix ,argname ',(cadr type))))

(defun generate-call-args (args &aux (macro (rest-arg-p args)))
  (loop for (type argname) in args
        if (eq (u:ensure-car type) :return)
          collect :pointer and
          collect (if macro `',argname argname)
        else
          unless (eq type '&rest)
            collect (if macro `',type type)
          end and
          collect argname))

(defun parse-args (args)
  (values (generate-lambda-list args)
          (generate-call-args args)
          (generate-foreign-objects args)
          (generate-foreign-objects args `(mem-aref))
          (rest-arg-p args)))

(defmacro defun/ift ((type struct) name return-type (&rest args) &optional docstring)
  (multiple-value-bind (lambda-list call-args returns-spec returns-read)
      (parse-args args)
    `(defun ,name (,type ,@lambda-list)
       ,docstring
       (with-foreign-objects (,@returns-spec)
         (values (foreign-funcall-pointer (foreign-slot-value (mem-aref ,type ',type)
                                                              '(:struct ,struct)
                                                              ',name)
                                          ()
                                          ,type ,type
                                          ,@call-args
                                          ,return-type)
                 ,@returns-read)))))

(defmacro defmacro/ift ((type struct) name return-type (&rest args) &optional docstring)
  (multiple-value-bind (lambda-list call-args returns-spec returns-read)
      (parse-args args)
    `(defmacro ,name (,type ,@lambda-list)
       ,docstring
       `(with-foreign-objects (,@',returns-spec)
          (values (foreign-funcall-pointer (foreign-slot-value (mem-aref ,,type ',',type)
                                                               '(:struct ,',struct)
                                                               ',',name)
                                           ()
                                           ,',type ,,type
                                           ,@(list* ,@call-args)
                                           ,',return-type)
                  ,@',returns-read)))))

(defmacro define-interface-function-table ((type-name struct-name) &body functors)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export '(,type-name ,struct-name ,@(mapcar #'car (remove-if-not #'listp functors)))
               ,(package-name *package*)))
     (defcstruct ,struct-name
       ,@(loop for slot in functors
               collect `(,(u:ensure-car slot) :pointer)))
     (defctype ,type-name (:pointer (:struct ,struct-name)))
     ,@(loop for functor in (remove-if-not #'listp functors)
             collect `(,(if (rest-arg-p (third functor)) 'defmacro/ift 'defun/ift)
                       (,type-name ,struct-name)
                       ,@functor))))
