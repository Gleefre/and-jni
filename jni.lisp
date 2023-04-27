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
           ((:string jll:string) (signature->string '(:class "java/lang/String")))))
        (t (ecase (first signature)
             ((:class) (format nil "L~a;" (signature->string (second signature))))
             ((:array) (format nil "[~a" (signature->string (second signature))))
             ((:method :function) (format nil "(~{~a~})~a"
                                          (mapcar #'signature->string (third signature))
                                          (signature->string (second signature))))))))

(defmacro sig (signature)
  "Same as signature->string but does not evaluate the argument."
  (signature->string signature))

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
         (field-id (jll:get-static-field-id env class name "Ljava/lang/String;"))
         (perm (jll:get-static-object-field env class field-id)))
    (jstring-to-string env perm)))
