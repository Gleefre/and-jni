(in-package #:and-jni/define-ift)

(defun generate-lambda-list (args)
  (loop with optional-off = t
        for (type argname . default-value) in args

        if (and default-value optional-off)
          collect '&optional and
          do (setf optional-off nil)
        else unless (or default-value optional-off)
          do (warn "No default value specified for argument after &optional. ~a"
                   (list* type argname default-value))

        unless (or (eq type :return)
                   (and (listp type)
                        (eq (car type) :return)))
          if optional-off
            collect argname
          else
            collect `(,argname ,@default-value)))

(defun generate-foreign-objects (args &optional prefix)
  (loop for (type argname) in args
        when (eq type :return)
        collect `(,@prefix ,argname :pointer)
        when (and (listp type)
                  (eq (car type) :return))
        collect `(,@prefix ,argname ',(cadr type))))

(defun generate-call-args (args &key quote-return)
  (loop for (type argname) in args
        if (eq type :return)
          collect :pointer and
          collect (if quote-return `',argname argname)
        else if (and (listp type)
                     (eq (car type) :return))
          collect :pointer and
          collect (if quote-return `',argname argname)
        else 
          collect type and
          collect argname))

(defmacro defun/ift ((type struct) name return-type (&rest args) &optional docstring)
  `(progn
     (defun ,name (,type ,@(generate-lambda-list args))
       ,docstring
       (with-foreign-objects (,@(generate-foreign-objects args))
         (values (foreign-funcall-pointer (foreign-slot-value (mem-aref ,type ',type)
                                                              '(:struct ,struct)
                                                              ',name)
                                          ()
                                          ,type ,type
                                          ,@(generate-call-args args)
                                          ,return-type)
                 ,@(generate-foreign-objects args `(mem-aref)))))))

(defun quote-odd (list)
  `(list ,@(loop for (a b) on list by #'cddr
                 collect `',a
                 collect b)))

(defmacro defmacro/ift ((type struct) name return-type (&rest args) &optional docstring)
  (let ((rest-args-name (cadar (last args)))
        (args (butlast args)))
    `(progn
       (defmacro ,name (,type ,@(generate-lambda-list args) &rest ,rest-args-name)
         ,docstring
         `(with-foreign-objects (,@',(generate-foreign-objects args))
            (values (foreign-funcall-pointer (foreign-slot-value (mem-aref ,,type ',',type)
                                                                 '(:struct ,',struct)
                                                                 ',',name)
                                             ()
                                             ,',type ,,type
                                             ,@,(quote-odd (generate-call-args args :quote-return t))
                                             ,@,rest-args-name
                                             ,',return-type)
                    ,@',(generate-foreign-objects args `(mem-aref))))))))

(defmacro define-interface-function-table ((type-name struct-name) &body functors)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export '(,type-name ,struct-name ,@(mapcar #'car (remove-if-not #'listp functors)))
               ,(package-name *package*)))
     (defcstruct ,struct-name
       ,@(loop for slot in functors
               collect `(,(if (listp slot)
                              (car slot)
                              slot)
                         :pointer)))
     (defctype ,type-name (:pointer (:struct ,struct-name)))
     ,@(mapcar (lambda (slot)
                 `(,(if (member '&rest (third slot) :key #'car) 'defmacro/ift 'defun/ift)
                   (,type-name ,struct-name) ,@slot))
               (remove-if-not #'listp functors))))
