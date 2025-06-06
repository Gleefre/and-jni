(defpackage #:and-jni/utils
  (:use #:cl)
  (:export #:ensure-car #:ensure-list))

(defpackage #:and-jni/parse-args
  (:use #:cl)
  (:local-nicknames (#:u #:and-jni/utils))
  (:export #:parse-args))

(defpackage #:and-jni/defcfun*
  (:use #:cl)
  (:import-from #:and-jni/parse-args #:parse-args)
  (:export #:defcfun*))

(defpackage #:and-jni/ift
  (:use #:cl)
  (:import-from #:and-jni/parse-args #:parse-args)
  (:local-nicknames (#:u #:and-jni/utils))
  (:export #:define-table #:define-function))

(defpackage #:and-jni/cffi
  (:use)
  (:import-from #:cl #:t #:&rest #:in-package)
  (:import-from #:cffi #:defcenum #:defcfun #:defcstruct #:defctype #:defcunion)
  (:import-from #:and-jni/defcfun* #:defcfun*)
  (:local-nicknames (#:ift #:and-jni/ift))
  (:export #:boolean #:byte #:char #:short #:int #:long #:float #:double #:size
           #:object #:class #:string #:array #:object-array
           #:boolean-array #:byte-array #:char-array #:short-array
           #:int-array #:long-array #:float-array #:double-array
           #:throwable #:weak #:value #:field-id #:method-id
           #:reference-type #:version #:code #:mode
           #:native-method #:vm-option #:vm-initargs #:vm-attach-args
           #:name #:signature #:function-pointer #:data #:group
           #:options #:options-number #:ignore-unrecognized
           #:get-default-vm-initargs #:create-vm #:get-created-vms))

(defpackage #:and-jni
  (:use #:cl)
  (:import-from #:cffi
                #:with-foreign-object #:with-foreign-objects
                #:foreign-slot-value
                #:mem-aref
                #:null-pointer)
  (:local-nicknames (#:jll #:and-jni/cffi) ; jll = jni low level
                    (#:a #:alexandria))
  (:export #:init
           #:get-default-vm-initargs
           #:create-vm
           #:get-created-vms
           #:signature->string
           #:sig
           #:*pending-exception*
           #:check-for-exception
           #:with-check-for-exception
           #:not-null
           #:call-java-method
           #:with-env

           #:jclass
           #:jmethod
           #:jcall
           #:jfield
           #:jstring
           #:jstring-to-string

           #:seq-to-jarray
           #:jarray
           #:jarray-to-list
           #:do-jarray

           #:jnew

           #:register-native-method
           #:define-native-method))

(defpackage #:and-jni/wrappers
  (:use #:cl)
  (:local-nicknames (#:jll #:and-jni/cffi)
                    (#:j   #:and-jni))
  (:export #:permission-name
           #:get-methods
           #:print-methods
           #:get-system-property
           #:set-system-property))
