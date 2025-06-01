(in-package #:and-jni/define-ift)

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

        unless (or (eq type :return)
                   (and (listp type)
                        (eq (car type) :return)))
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
  `(defun ,name (,type ,@(generate-lambda-list args))
     ,docstring
     (with-foreign-objects (,@(generate-foreign-objects args))
       (values (foreign-funcall-pointer (foreign-slot-value (mem-aref ,type ',type)
                                                            '(:struct ,struct)
                                                            ',name)
                                        ()
                                        ,type ,type
                                        ,@(generate-call-args args)
                                        ,return-type)
               ,@(generate-foreign-objects args `(mem-aref))))))

(defun quote-odd (list)
  `(list ,@(loop for (a b) on list by #'cddr
                 collect `',a
                 collect b)))

(defmacro defmacro/ift ((type struct) name return-type (&rest args) &optional docstring)
  `(defmacro ,name (,type ,@(generate-lambda-list args))
     ,docstring
     `(with-foreign-objects (,@',(generate-foreign-objects args))
        (values (foreign-funcall-pointer (foreign-slot-value (mem-aref ,,type ',',type)
                                                             '(:struct ,',struct)
                                                             ',',name)
                                         ()
                                         ,',type ,,type
                                         ,@,(quote-odd (generate-call-args (butlast args) :quote-return t))
                                         ,@,(second (car (last args)))
                                         ,',return-type)
                ,@',(generate-foreign-objects args `(mem-aref))))))

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
