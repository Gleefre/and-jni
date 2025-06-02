(in-package #:and-jni)

(defun get-default-vm-initargs (&optional (vm-version :v1.6))
  (when (eq vm-version :v1.1)
    (error "AND-JNI doesn't support ~S version" vm-version))
  (with-foreign-object (ret-vm-initargs '(:struct jll:vm-initargs))
    (setf (foreign-slot-value ret-vm-initargs
                              '(:struct jll:vm-initargs)
                              'jll:version)
          vm-version)
    (let ((status (jll:get-default-vm-initargs ret-vm-initargs)))
      (values (when (eq status :ok)
                (foreign-slot-value ret-vm-initargs
                                    '(:struct jll:vm-initargs)
                                    'jll:version))
              status))))

(defun create-vm (&key (vm-version :v1.6)
                       options
                       (ignore-unrecognized t))
  (with-foreign-objects ((vm-initargs '(:struct jll:vm-initargs))
                         (vm-options '(:struct jll:vm-option) (length options)))
    (loop for (name data) in options
          for i from 0
          do (setf (mem-aref vm-options '(:struct jll:vm-option) i)
                   `(jll:name ,name jll:data ,(or data (null-pointer)))))
    (setf (mem-aref vm-initargs '(:struct jll:vm-initargs))
          `(jll:version ,vm-version jll:options-number ,(length options)
            jll:options ,vm-options jll:ignore-unrecognized ,(if ignore-unrecognized 1 0)))
    (jll:create-vm vm-initargs)))

(defun get-created-vms (&optional (buffer-length 1))
  (with-foreign-objects ((return-vms '(:pointer jll:vm) buffer-length)
                         (return-number '(:pointer jll:size)))
    (let ((status (jll:get-created-vms return-vms buffer-length return-number)))
      (values (when (eq status :ok)
                (loop for i below (min (mem-aref return-number 'jll:size)
                                       buffer-length)
                      collect (mem-aref return-vms 'jll:vm i)))
              status))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun signature->string (signature)
    "Transforms signature represented as a tree to a string signature"
    (cond ((stringp signature) signature)
          ((atom signature)
           (ecase signature
             ((:boolean jll:boolean) "Z")
             ((:byte jll:byte) "B")
             ((:char jll:char) "C")
             ((:short jll:short) "S")
             ((:int jll:int) "I")
             ((:long jll:long) "J")
             ((:float jll:float) "F")
             ((:double jll:double) "D")
             ((:void) "V")
             ((:string jll:string) (signature->string '(:class "java/lang/String")))))
          (t (ecase (first signature)
               ((:class) (format nil "L~a;" (signature->string (second signature))))
               ((:array) (format nil "[~a" (signature->string (second signature))))
               ((:method :function) (format nil "(~{~a~})~a"
                                            (mapcar #'signature->string (third signature))
                                            (signature->string (second signature))))))))

  (defmacro sig (signature)
    "Same as signature->string but does not evaluate the argument."
    (signature->string signature)))

;; Call with checking for exception

(defvar *pending-exception*)

(defun check-for-exception (env)
  (unless (zerop (jll:exception-check env))
    (unwind-protect (progn (jll:exception-describe env)
                           (let ((*pending-exception* (jll:exception-occurred env)))
                             (unless (cffi:null-pointer-p *pending-exception*)
                               (error "Java exception occurred (see *pending-exception*)"))))
      (jll:exception-clear env))))

(defmacro with-check-for-exception (env expr &rest exprs)
  `(multiple-value-prog1 (progn ,expr ,@exprs) (check-for-exception ,env)))

;; Ensure that returned value is not NULL

(defmacro not-null (&body body)
  (a:with-gensyms ($result)
    `(let ((,$result (progn ,@body)))
       (when (cffi:null-pointer-p ,$result)
         (error "Java method returned null."))
       ,$result)))

;; call-java-method utilities

(defun caller (ret-type &optional is-static)
  (find-symbol (concatenate 'string
                            "CALL"
                            (if is-static
                                "-STATIC"
                                "")
                            "-"
                            (case ret-type
                              ((:boolean jll:boolean) "BOOLEAN")
                              ((:byte jll:byte) "BYTE")
                              ((:char jll:char) "CHAR")
                              ((:short jll:short) "SHORT")
                              ((:int jll:int) "INT")
                              ((:long jll:long) "LONG")
                              ((:float jll:float) "FLOAT")
                              ((:double jll:double) "DOUBLE")
                              ((:void) "VOID")
                              (t "OBJECT"))
                            "-METHOD")
               '#:and-jni/cffi))

(defun ensure-java-type (type)
  (etypecase type
    (string 'jll:object)
    (symbol (find-symbol (symbol-name type) :jll))))

(defun type-for-signature (type)
  (etypecase type
    (symbol type)
    (string `(:class ,type))))

;; call-java-method: macro for simplified method calls

(defmacro call-java-method (env (class &optional instance) method ret-type &rest type-arg-pairs)
  (let ((method-signature (signature->string `(:method ,(type-for-signature ret-type)
                                                ,(mapcar (a:compose #'type-for-signature #'car)
                                                         type-arg-pairs)))))
    (a:with-gensyms ($class $method $env $class-name $method-name $instance)
      `(let* ((,$env ,env)
              (,$class-name ,class)
              ,@(when instance `((,$instance ,instance)))
              (,$method-name ,method)
              (,$class (not-null
                         (with-check-for-exception ,$env
                           (jll:find-class ,$env ,$class-name))))
              (,$method (not-null
                          (with-check-for-exception ,$env
                            (,(if instance 'jll:get-method-id 'jll:get-static-method-id)
                             ,$env ,$class ,$method-name ,method-signature)))))
         (with-check-for-exception ,$env
           (,(caller ret-type (not instance))
            ,$env ,(if instance $instance $class) ,$method
            ,@(loop for (type arg) in type-arg-pairs
                    collect (ensure-java-type type)
                    collect arg)))))))

;; with-environment gets an environment if thread is attached,
;; or attaches and detaches after the usage.

(defmacro with-env ((&optional (var (gensym "JENV")) version) &body body)
  (alexandria:with-gensyms ($vms $status)
    `(multiple-value-bind (,$vms ,$status) (get-created-vms)
       (unless (eq :ok ,$status)
         (error "Error occurred while fetching JVMs: ~s" ,$status))
       (unless (plusp (length ,$vms))
         (error "No JVM created"))
       (multiple-value-bind (,$status ,var) (jll:get-env (car ,$vms) ,@(when version `(,version)))
         (declare (ignorable ,var))
         (case ,$status
           ((:ok)
            (progn ,@body))
           ((:thread-detached-from-vm)
            (multiple-value-bind (,$status ,var) (jll:attach-current-thread (car ,$vms))
              (declare (ignorable ,var))
              (unless (eq :ok ,$status)
                (error "Error occurred during jll:attach-current-thread : ~s" ,$status))
              (unwind-protect (progn ,@body)
                (jll:detach-current-thread (car ,$vms)))))
           (t (error "Error occurred during jll:get-env : ~s" ,$status)))))))

;;; More high level stuff

(defun jclass (class-name)
  (with-env (env)
    (with-check-for-exception env
      (jll:find-class env class-name))))

(defun %jmethod (class name signature)
  (with-env (env)
    (with-check-for-exception env
      (jll:get-method-id env class name signature))))

(defun %jmethod-static (class name signature)
  (with-env (env)
    (with-check-for-exception env
      (jll:get-static-method-id env class name signature))))

(defmacro jmethod (class name &rest &static/return-type/arg-types
                   &aux (args &static/return-type/arg-types))
  (assert (not (null args)))
  (let ((static (and (eql :static (car args))
                     (pop args))))
    (assert (not (null args)))
    (destructuring-bind (return-type &rest arg-types) args
      `(with-env ()
         (,(if static
               '%jmethod-static
               '%jmethod)
          ,(if (stringp class)
               `(jclass ,class)
               class)
          ,name
          (sig (:method ,return-type (,@arg-types))))))))

(defun to-cffi-type (type)
  (case type
    ((:boolean jll:boolean) 'jll:boolean)
    ((:byte jll:byte) 'jll:byte)
    ((:char jll:char) 'jll:char)
    ((:short jll:short) 'jll:short)
    ((:int jll:int) 'jll:int)
    ((:long jll:long) 'jll:long)
    ((:float jll:float) 'jll:float)
    ((:double jll:double) 'jll:double)
    (:void :void)
    (t 'jll:object)))

(defmacro jcall (ret-type (class method) static-or-object &rest type-arg-pairs)
  (assert (evenp (length type-arg-pairs)))
  (let (($class (gensym "JCLASS"))
        ($method (gensym "JMETHOD"))
        (static-p (eq static-or-object :static)))
    `(with-env (env)
       (let ((,$class ,class)
             (,$method ,method))
         (when (stringp ,$class)
           (setf ,$class (jclass ,$class)))
         (when (stringp ,$method)
           (setf ,$method (jmethod ,$class
                                   ,method
                                   ,@(when static-p
                                       '(:static))
                                   ,ret-type
                                   ,@(loop for (type) on type-arg-pairs by #'cddr
                                           collect type))))
         (,(caller ret-type static-p)
          env
          ,(if static-p $class static-or-object)
          ,$method
          ,@(loop for (type arg) on type-arg-pairs by #'cddr
                  collect (to-cffi-type type)
                  collect arg))))))

(defun field-getter (type is-static)
  (find-symbol (concatenate 'string
                            "GET"
                            (if is-static
                                "-STATIC"
                                "")
                            "-"
                            (case type
                              ((:boolean jll:boolean) "BOOLEAN")
                              ((:byte jll:byte) "BYTE")
                              ((:char jll:char) "CHAR")
                              ((:short jll:short) "SHORT")
                              ((:int jll:int) "INT")
                              ((:long jll:long) "LONG")
                              ((:float jll:float) "FLOAT")
                              ((:double jll:double) "DOUBLE")
                              (t "OBJECT"))
                            "-FIELD")
               '#:and-jni/cffi))

(defmacro jfield (type (class field) static-or-object)
  (let (($class (gensym "JCLASS"))
        ($field (gensym "JFIELD"))
        (static-p (eq static-or-object :static)))
    `(with-env (env)
       (let ((,$class ,class)
             (,$field ,field))
         (when (stringp ,$class)
           (setf ,$class (jclass ,$class)))
         (when (stringp ,$field)
           (setf ,$field
                 (,(if static-p
                       'jll:get-static-field-id
                       'jll:get-field-id)
                  env
                  ,$class
                  ,$field
                  (sig ,type))))
         (,(field-getter type static-p)
          env
          ,(if static-p $class static-or-object)
          ,$field)))))

;;; Arrays

(defparameter +primitive-types+
  '(:boolean jll:boolean
    :byte jll:byte
    :char jll:char
    :short jll:short
    :int jll:int
    :long jll:long
    :float jll:float
    :double jll:double))

(defun primitive-type-p (type)
  (member type +primitive-types+))

(defun jarray-constructor (type)
  (ecase type
    ((:boolean jll:boolean) #'jll:new-boolean-array)
    ((:byte jll:byte) #'jll:new-byte-array)
    ((:char jll:char) #'jll:new-char-array)
    ((:short jll:short) #'jll:new-short-array)
    ((:int jll:int) #'jll:new-int-array)
    ((:long jll:long) #'jll:new-long-array)
    ((:float jll:float) #'jll:new-float-array)
    ((:double jll:double) #'jll:new-double-array)))

(defun jarray-elements-getter (type)
  (ecase type
    ((:boolean jll:boolean) #'jll:get-boolean-array-elements)
    ((:byte jll:byte) #'jll:get-byte-array-elements)
    ((:char jll:char) #'jll:get-char-array-elements)
    ((:short jll:short) #'jll:get-short-array-elements)
    ((:int jll:int) #'jll:get-int-array-elements)
    ((:long jll:long) #'jll:get-long-array-elements)
    ((:float jll:float) #'jll:get-float-array-elements)
    ((:double jll:double) #'jll:get-double-array-elements)))

(defun jarray-elements-releaser (type)
  (ecase type
    ((:boolean jll:boolean) #'jll:release-boolean-array-elements)
    ((:byte jll:byte) #'jll:release-byte-array-elements)
    ((:char jll:char) #'jll:release-char-array-elements)
    ((:short jll:short) #'jll:release-short-array-elements)
    ((:int jll:int) #'jll:release-int-array-elements)
    ((:long jll:long) #'jll:release-long-array-elements)
    ((:float jll:float) #'jll:release-float-array-elements)
    ((:double jll:double) #'jll:release-double-array-elements)))

(defun seq-to-primitive-jarray (type seq &aux (length (length seq)))
  (with-env (env)
    (let* ((jarray (funcall (jarray-constructor type) env length))
           (jarray-buffer (funcall (jarray-elements-getter type) env jarray)))
      (loop for i from 0
            for element in (coerce seq 'list)
            do (setf (cffi:mem-aref jarray-buffer (to-cffi-type type) i)
                     element))
      (funcall (jarray-elements-releaser type) env jarray jarray-buffer :release)
      jarray)))

(defun seq-to-object-jarray (class seq &aux (length (length seq)))
  (with-env (env)
    (let ((jarray (jll:new-object-array env
                                        length
                                        (if (stringp class)
                                            (jclass class)
                                            class)
                                        (cffi:null-pointer))))
      (loop for i from 0
            for element in (coerce seq 'list)
            do (jll:set-object-array-element env jarray i element))
      jarray)))

(defun seq-to-jarray (class seq)
  (if (primitive-type-p class)
      (seq-to-primitive-jarray class seq)
      (seq-to-object-jarray class seq)))

(defun jarray (class &rest elements)
  (seq-to-jarray class elements))

(defun primitive-jarray-to-list (jarray type)
  (with-env (env)
    (let* ((length (jll:get-array-length env jarray))
           (jarray-buffer (funcall (jarray-elements-getter type) env jarray)))
      (prog1 (loop for i below length
                   collect (cffi:mem-aref jarray-buffer (to-cffi-type type) i))
        (funcall (jarray-elements-releaser type) env jarray jarray-buffer :release)))))

(defun object-jarray-to-list (jarray)
  (with-env (env)
    (let* ((length (jll:get-array-length env jarray)))
      (loop for i below length
            collect (jll:get-object-array-element env jarray i)))))

(defun jarray-to-list (jarray &optional (type 'jll:object))
  (if (primitive-type-p type)
      (primitive-jarray-to-list jarray type)
      (object-jarray-to-list jarray)))

(defmacro do-jarray ((var jarray &optional result) &body body)
  `(with-env ()
     (dolist (,var (jarray-to-list ,jarray) ,result)
       ,@body)))

;;; Strings
;;; TODO: What about modified UTF8 used by JVM ?

(defun jstring (string)
  (with-env (env)
    (jll:new-string-utf env string)))

(defun jstring-to-string (jstring)
  (if (cffi:null-pointer-p jstring)
      (error "NULL is not a valid string")
      (with-env (env)
        (let* ((length (jll:get-string-utf-length env jstring))
               (chars (jll:get-string-utf-chars env jstring)))
          (unwind-protect (coerce (loop for i below length
                                        collect (code-char (cffi:mem-aref chars :char i)))
                                  'string)
            (jll:release-string-utf-chars env jstring chars))))))

;;; Constructing java objects

(defmacro jnew (class &rest type-arg-pairs
                &aux ($class (gensym "JCLASS"))
                     ($constructor (gensym "CONSTRUCTOR")))
  (assert (evenp (length type-arg-pairs)))
  `(with-env (env)
     (let* ((,$class (jclass ,class))
            (,$constructor (jmethod ,$class "<init>" :void ,@(loop for (type nil) on type-arg-pairs by #'cddr
                                                                   collect type))))
       (jll:new-object env ,$class ,$constructor ,@(loop for (type arg) on type-arg-pairs by #'cddr
                                                           collect (to-cffi-type type)
                                                           collect arg)))))

;;; Native methods

(defun register-native-method (class name signature function-pointer)
  (with-env (env)
    (cffi:with-foreign-object (native-method '(:struct jll:native-method))
      (setf (cffi:mem-aref native-method '(:struct jll:native-method))
            `(jll:name ,name jll:signature ,signature jll:function-pointer ,function-pointer))
      (jll:register-natives env (jclass class) native-method 1))))

(defmacro define-native-method (class name ret-type args
                                &body body
                                &aux (callback-name (gensym "JNATIVE-METHOD"))
                                     ($env (gensym "JENV")))
  `(progn
     (cffi:defcallback ,callback-name
         ,(to-cffi-type ret-type)
         ((,$env jll:env)
          (this jll:object)
          ,@(loop for (var type) in args
                  collect `(,var ,(to-cffi-type type))))
       (declare (ignore ,$env) (ignorable this))
       ,@body)
     (register-native-method ,class ,name (sig (:method ,ret-type
                                                 (,@(loop for (nil type) in args
                                                          collect type))))
                             (cffi:callback ,callback-name))))
