(defpackage #:and-jni/define-ift
  (:use #:cl #:cffi)
  (:export #:define-interface-function-table))

(defpackage #:and-jni/cffi
  (:use #:and-jni/define-ift)
  (:import-from #:cl #:t #:&rest #:in-package)
  (:import-from #:cffi
                #:defctype #:defcstruct #:defcenum
                #:defcunion #:defcfun #:null-pointer)
  (:export #:boolean #:byte #:char #:short #:int #:long #:float #:double
           #:size
           #:object #:class #:string #:array
           #:object-array #:boolean-array #:byte-array #:char-array #:short-array
           #:int-array #:long-array #:float-array #:double-array
           #:throwable #:weak
           #:value
           #:field-id #:method-id
           #:reference-type #:version #:code #:mode
           #:native-method #:vm-option #:vm-initargs #:vm-attach-args
           #:%get-default-vm-initargs #:%create-vm #:%get-created-vms
           #:options #:options-number #:ignore-unrecognized #:name #:data))

(defpackage #:and-jni
  (:use #:cl #:cffi)
  (:local-nicknames (#:jll #:and-jni/cffi))  ; jll = jni low level
  (:export #:init
           #:get-default-vm-initargs
           #:create-vm
           #:get-created-vms))
