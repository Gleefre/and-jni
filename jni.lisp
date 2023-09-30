(in-package #:and-jni)

(defun get-default-vm-initargs (&optional (vm-version :v1.6))
  (with-foreign-object (ret-vm-initargs '(:struct jll:vm-initargs))
    (setf (foreign-slot-value ret-vm-initargs
                              '(:struct jll:vm-initargs)
                              'jll:version)
          vm-version)
    (let ((status (jll:%get-default-vm-initargs ret-vm-initargs)))
      (values (when (eq status :ok)
                (mem-aref ret-vm-initargs '(:struct jll:vm-initargs)))
              status))))

(defun create-vm (&key (vm-version :v1.6)
                       options
                       (ignore-unrecognized t))
  (with-foreign-objects ((vm-initargs '(:struct jll:vm-initargs))
                         (vm-options '(:struct jll:vm-option) (length options))
                         (ret-vm :pointer)
                         (ret-env :pointer))
    (loop for (name data) in options
          for i from 0
          do (setf (mem-aref vm-options '(:struct jll:vm-option) i)
                   `(jll:name ,name jll:data ,(or data (null-pointer)))))
    (setf (mem-aref vm-initargs '(:struct jll:vm-initargs))
          `(jll:version ,vm-version jll:options-number ,(length options)
            jll:options ,vm-options jll:ignore-unrecognized ,(if ignore-unrecognized 1 0)))
    (values (jll:%create-vm ret-vm ret-env vm-initargs)
            (mem-aref ret-vm :pointer)
            (mem-aref ret-env :pointer))))

(defun get-created-vms (&optional (buffer-length 1))
  (with-foreign-objects ((return-vms '(:pointer jll:vm) buffer-length)
                         (return-number '(:pointer jll:size)))
    (let ((status (jll:%get-created-vms return-vms buffer-length return-number)))
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
                            (etypecase ret-type
                              (string "OBJECT")
                              (symbol (if (string= "STRING" (symbol-name ret-type))
                                          "OBJECT"
                                          (symbol-name ret-type))))
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

(defmacro with-env ((var &optional version) &body body)
  (alexandria:with-gensyms ($vms $status)
    `(multiple-value-bind (,$vms ,$status) (get-created-vms)
       (unless (eq :ok ,$status)
         (error "Error occurred while fetching JVMs: ~s" ,$status))
       (unless (plusp (length ,$vms))
         (error "No JVM created"))
       (multiple-value-bind (,$status ,var) (jll:get-env (car ,$vms) ,@(when version `(,version)))
         (case ,$status
           ((:ok)
            (progn ,@body))
           ((:thread-detached-from-vm)
            (multiple-value-bind (,$status ,var) (jll:attach-current-thread (car ,$vms))
              (unless (eq :ok ,$status)
                (error "Error occurred during jll:attach-current-thread : ~s" ,$status))
              (unwind-protect (progn ,@body)
                (jll:detach-current-thread (car ,$vms)))))
           (t (error "Error occurred during jll:get-env : ~s" ,$status)))))))

;; Some stuff

(defun jstring-to-string (env jstring)
  (let* ((length (jll:get-string-utf-length env jstring))
         (chars (jll:get-string-utf-chars env jstring)))
    (unwind-protect (coerce (loop for i below length
                                  collect (code-char (cffi:mem-aref chars :char i)))
                            'string)
      (jll:release-string-utf-chars env jstring chars))))

(defun permission-name (env name)
  (let* ((class (jll:find-class env "android/Manifest$permission"))
         (field-id (jll:get-static-field-id env class name (sig :string)))
         (perm (jll:get-static-object-field env class field-id)))
    (jstring-to-string env perm)))
