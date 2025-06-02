(in-package #:and-jni/cffi)

;;;; From jni.h
;;;; See http://java.sun.com/javase/6/docs/technotes/guides/jni/spec/jniTOC.html
;;;; Newer version: https://docs.oracle.com/en/java/javase/21/docs/specs/jni/index.html

;; Primitive types that match up with Java equivalents.
(defctype boolean :uint8)
(defctype byte :int8)
(defctype char :uint16)
(defctype short :int16)
(defctype int :int32)
(defctype long :int64)
(defctype float :float)
(defctype double :double)

;; Cardinal indices and sizes.
(defctype size int)

;; Reference types, in C.
(defctype object :pointer)
(defctype class object)
(defctype string object)
(defctype array object)
(defctype object-array array)
(defctype boolean-array array)
(defctype byte-array array)
(defctype char-array array)
(defctype short-array array)
(defctype int-array array)
(defctype long-array array)
(defctype float-array array)
(defctype double-array array)
(defctype throwable object)

;; Field and Method IDs
(defctype field-id :pointer)
(defctype method-id :pointer)

;; Value type
(defcunion value
  (boolean boolean)
  (byte byte)
  (char char)
  (short short)
  (int int)
  (long long)
  (float float)
  (double double)
  (object object))

;; Is not described in documentation, but is used in NewWeakGlobalRef
;; and DeleteWeakGlobalRef functions.
(defctype weak object)

;;; Some enums
(defcenum reference-type
  :invalid
  :local
  :global
  :weak-global)

(defcenum (version :int :allow-undeclared-values t)
  (:v1.1 #x00010001)
  (:v1.2 #x00010002)
  (:v1.4 #x00010004)
  (:v1.6 #x00010006)
  (:v1.8 #x00010008)
  (:v9   #x00090000)
  (:v10  #x000a0000)
  (:v19  #x00130000)
  (:v20  #x00140000)
  (:v21  #x00150000)
  (:v24  #x00180000))

(defcenum (code :int :allow-undeclared-values t)
  (:ok 0)
  (:unknown-error -1)
  (:thread-detached-from-vm -2)
  (:jni-version-error -3)
  (:not-enough-memory -4)
  (:vm-already-created -5)
  (:invalid-arguments -6))

(defcenum (mode :int)
  :release
  :commit
  :abort)

;;; Additional structures

(defcstruct native-method
  (name :string)
  (signature :string)
  (function-pointer :pointer))

(defcstruct vm-option
  (name :string)
  (data :pointer))

(defcstruct vm-initargs
  (version version)
  (options-number int)
  (options (:pointer (:struct vm-option)))
  (ignore-unrecognized boolean))

(defcstruct vm-attach-args
  (version version)
  (name :string)
  (group object))

;;; Interface Function Tables

(ift:define-table (vm invocation-interface :export t)
  %reserved
  %reserved
  %reserved
  (destroy-vm code ()
    "Unloads a Java VM and reclaims its resources.
Waits until the current thread is the only non-daemon user-level Java thread.")
  (attach-current-thread code ((:return env)
                               ((:pointer (:struct vm-attach-args)) args (cffi:null-pointer)))
    "Attaches the current thread to a Java VM.")
  (detach-current-thread code ()
    "Detaches the current thread from a Java VM.")
  (get-env code ((:return env) (version version :v1.6))
    "Returns Java Environment as second value.")
  (attach-current-thread-as-daemon code ((:return env)
                                         ((:pointer (:struct vm-attach-args)) args (cffi:null-pointer)))
    "Attaches the current thread to a Java VM as a daemon."))

;; See https://docs.oracle.com/javase/6/docs/technotes/guides/jni/spec/functions.html
(ift:define-table (env native-interface :export t)
  %reserved
  %reserved
  %reserved
  %reserved

  (get-version version ()
    "Returns the version of the native method interface.")
  (define-class class ((:string name) (object loader) ((:pointer byte) buffer) (size buffer-length))
    "Loads a class from a buffer of raw class data.")
  (find-class class ((:string name))
    "Returns a class object from a fully-qualified name, or NULL if the class cannot be found.")

  (from-reflected-method method-id ((object method))
    "Converts a java.lang.reflect object to a method ID.")
  (from-reflected-field field-id ((object field))
    "Converts a java.lang.reflect.Field to a field ID.")
  ;; spec doesn't show boolean parameter
  (to-reflected-method object ((class class) (method-id method-id) (boolean is-static))
    "Converts method ID derived from class to a java.lang.reflect object.")

  (get-superclass class ((class class))
    "Returns the object that represents the superclass of the class, or NULL if the superclass cannot be found.")
  (is-assignable-from boolean ((class class-1) (class class-2))
    "Determines whether an object of class-1 can be safely cast to class-2.")

  ;; spec doesn't show boolean parameter
  (to-reflected-field object ((class class) (field-id field-id) (boolean is-static))
    "Converts a field ID derived from class to a java.lang.reflect.Field object.")

  (throw code ((throwable condition))
    "Causes a java.lang.Throwable object to be thrown.")
  (throw-new code ((class class) (:string message))
    "Constructs an exception object from the class with message and causes that exception to be thrown.")
  (exception-occurred throwable ()
    "Determines if an exception is being thrown.")
  (exception-describe :void ()
    "Prints an exception and a backtrace of the stack to a system error-reporting channel (ex. stderr).")
  (exception-clear :void ()
    "Clears any exception that is currently being thrown.")
  (fatal-error :void ((:string message))
    "Raises a fatal error and does not expect the VM to recover.
This function does not return.")

  (push-local-frame code ((int capacity))
    "Creates a new local reference frame, in which at least a given number of local references can be created.
Returns :ok on success, an error code and a pending OutOfMemoryError on failure.")
  (pop-local-frame object ((object result))
    "Pops off the current local reference frame, frees all the local references.
Returns a local reference in the previous local reference frame for the given result object.
Pass NULL as result if you do not need to return a reference to the previous frame.")

  (new-global-reference object ((object object))
    "Creates a new global reference to the object.
Returns NULL if the system runs out of memory.")
  (delete-global-reference :void ((object global-reference))
    "Deletes the global reference pointed to by global-reference.")
  (delete-local-reference :void ((object local-reference))
    "Deletes the local reference pointed to by local-reference.")
  (is-same-object boolean ((object reference-1) (object reference-2))
    "Tests whether two references refer to the same Java object.")
  (new-local-reference object ((object reference))
    "Creates a new local reference that refers to the same object as reference.
Returns NULL if ref refers to null.")
  (ensure-local-capacity code ((int capacity))
    "Ensures that at least a given number of local references can be created in the current thread.
Returns :ok on success, an error code and a pending OutOfMemoryError on failure.")

  (alloc-object object ((class class))
    "Allocates a new Java object without invoking any of the constructors for the object.
Returns a reference to the object.
The class must not refer to an array class.")
  (new-object object ((class class) (method-id method-id) (&rest args))
    "Constructs a new Java object and invokes given constructor method.
This ID must be obtained by calling get-method-id.")
  %new-object-v
  (new-object-a object ((class class) (method-id method-id) ((:pointer (:union value)) args))
    "Constructs a new Java object and invokes given constructor method.
This ID must be obtained by calling get-method-id.")

  (get-object-class class ((object object))
    "Returns the class of an object. Object must not be NULL.")
  (is-instance-of boolean ((object object) (class class))
    "Tests whether an object is an instance of a class.")

  (get-method-id method-id ((class class) (:string name) (:string signature))
    "Returns the method ID for an instance (nonstatic) method of a class or interface,
or NULL if the specified method cannot be found.
Causes an uninitialized class to be initialized.")

  (call-object-method object ((object object) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-object-method-v
  (call-object-method-a object ((object object) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-boolean-method boolean ((object object) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-boolean-method-v
  (call-boolean-method-a boolean ((object object) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-byte-method byte ((object object) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-byte-method-v
  (call-byte-method-a byte ((object object) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-char-method char ((object object) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-char-method-v
  (call-char-method-a char ((object object) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-short-method short ((object object) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-short-method-v
  (call-short-method-a short ((object object) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-int-method int ((object object) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-int-method-v
  (call-int-method-a int ((object object) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-long-method long ((object object) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-long-method-v
  (call-long-method-a long ((object object) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-float-method float ((object object) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-float-method-v
  (call-float-method-a float ((object object) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-double-method double ((object object) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-double-method-v
  (call-double-method-a double ((object object) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-void-method :void ((object object) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-void-method-v
  (call-void-method-a :void ((object object) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")

  (call-nonvirtual-object-method object ((object object) (class class) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-nonvirtual-object-method-v
  (call-nonvirtual-object-method-a object ((object object) (class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-nonvirtual-boolean-method boolean ((object object) (class class) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-nonvirtual-boolean-method-v
  (call-nonvirtual-boolean-method-a boolean ((object object) (class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-nonvirtual-byte-method byte ((object object) (class class) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-nonvirtual-byte-method-v
  (call-nonvirtual-byte-method-a byte ((object object) (class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-nonvirtual-char-method char ((object object) (class class) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-nonvirtual-char-method-v
  (call-nonvirtual-char-method-a char ((object object) (class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-nonvirtual-short-method short ((object object) (class class) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-nonvirtual-short-method-v
  (call-nonvirtual-short-method-a short ((object object) (class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-nonvirtual-int-method int ((object object) (class class) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-nonvirtual-int-method-v
  (call-nonvirtual-int-method-a int ((object object) (class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-nonvirtual-long-method long ((object object) (class class) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-nonvirtual-long-method-v
  (call-nonvirtual-long-method-a long ((object object) (class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-nonvirtual-float-method float ((object object) (class class) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-nonvirtual-float-method-v
  (call-nonvirtual-float-method-a float ((object object) (class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-nonvirtual-double-method double ((object object) (class class) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-nonvirtual-double-method-v
  (call-nonvirtual-double-method-a double ((object object) (class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")
  (call-nonvirtual-void-method :void ((object object) (class class) (method-id method-id) (&rest args))
    "Returns the result of calling the Java method.")
  %call-nonvirtual-void-method-v
  (call-nonvirtual-void-method-a :void ((object object) (class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the Java method.")

  (get-field-id field-id ((class class) (:string name) (:string signature))
    "Returns the field ID for an instance (nonstatic) field of a class, or NULL if the operation fails.
Causes an uninitialized class to be initialized.

get-field-id cannot be used to obtain the length field of an array.
Use get-array-length instead.")

  (get-object-field object ((object object) (field-id field-id))
    "Returns the content of the field.")
  (get-boolean-field boolean ((object object) (field-id field-id))
    "Returns the content of the field.")
  (get-byte-field byte ((object object) (field-id field-id))
    "Returns the content of the field.")
  (get-char-field char ((object object) (field-id field-id))
    "Returns the content of the field.")
  (get-short-field short ((object object) (field-id field-id))
    "Returns the content of the field.")
  (get-int-field int ((object object) (field-id field-id))
    "Returns the content of the field.")
  (get-long-field long ((object object) (field-id field-id))
    "Returns the content of the field.")
  (get-float-field float ((object object) (field-id field-id))
    "Returns the content of the field.")
  (get-double-field double ((object object) (field-id field-id))
    "Returns the content of the field.")
  (set-object-field :void ((object object) (field-id field-id) (object value))
    "Sets the value of an instance (nonstatic) field of an object.")
  (set-boolean-field :void ((object object) (field-id field-id) (boolean value))
    "Sets the value of an instance (nonstatic) field of an object.")
  (set-byte-field :void ((object object) (field-id field-id) (byte value))
    "Sets the value of an instance (nonstatic) field of an object.")
  (set-char-field :void ((object object) (field-id field-id) (char value))
    "Sets the value of an instance (nonstatic) field of an object.")
  (set-short-field :void ((object object) (field-id field-id) (short value))
    "Sets the value of an instance (nonstatic) field of an object.")
  (set-int-field :void ((object object) (field-id field-id) (int value))
    "Sets the value of an instance (nonstatic) field of an object.")
  (set-long-field :void ((object object) (field-id field-id) (long value))
    "Sets the value of an instance (nonstatic) field of an object.")
  (set-float-field :void ((object object) (field-id field-id) (float value))
    "Sets the value of an instance (nonstatic) field of an object.")
  (set-double-field :void ((object object) (field-id field-id) (double value))
    "Sets the value of an instance (nonstatic) field of an object.")

  (get-static-method-id method-id  ((class class) (:string name) (:string signature))
    "Returns the method ID for a static method of a class, or NULL if the operation fails.
Causes an uninitialized class to be initialized.")

  (call-static-object-method object ((class class) (method-id method-id) (&rest args))
    "Returns the result of calling the static Java method.")
  %call-static-object-method-v
  (call-static-object-method-a object ((class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the static Java method.")
  (call-static-boolean-method boolean ((class class) (method-id method-id) (&rest args))
    "Returns the result of calling the static Java method.")
  %call-static-boolean-method-v
  (call-static-boolean-method-a boolean ((class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the static Java method.")
  (call-static-byte-method byte ((class class) (method-id method-id) (&rest args))
    "Returns the result of calling the static Java method.")
  %call-static-byte-method-v
  (call-static-byte-method-a byte ((class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the static Java method.")
  (call-static-char-method char ((class class) (method-id method-id) (&rest args))
    "Returns the result of calling the static Java method.")
  %call-static-char-method-v
  (call-static-char-method-a char ((class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the static Java method.")
  (call-static-short-method short ((class class) (method-id method-id) (&rest args))
    "Returns the result of calling the static Java method.")
  %call-static-short-method-v
  (call-static-short-method-a short ((class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the static Java method.")
  (call-static-int-method int ((class class) (method-id method-id) (&rest args))
    "Returns the result of calling the static Java method.")
  %call-static-int-method-v
  (call-static-int-method-a int ((class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the static Java method.")
  (call-static-long-method long ((class class) (method-id method-id) (&rest args))
    "Returns the result of calling the static Java method.")
  %call-static-long-method-v
  (call-static-long-method-a long ((class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the static Java method.")
  (call-static-float-method float ((class class) (method-id method-id) (&rest args))
    "Returns the result of calling the static Java method.")
  %call-static-float-method-v
  (call-static-float-method-a float ((class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the static Java method.")
  (call-static-double-method double ((class class) (method-id method-id) (&rest args))
    "Returns the result of calling the static Java method.")
  %call-static-double-method-v
  (call-static-double-method-a double ((class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the static Java method.")
  (call-static-void-method :void ((class class) (method-id method-id) (&rest args))
    "Returns the result of calling the static Java method.")
  %call-static-void-method-v
  (call-static-void-method-a :void ((class class) (method-id method-id) ((:pointer (:union value)) args))
    "Returns the result of calling the static Java method.")

  (get-static-field-id field-id ((class class) (:string name) (:string signature))
    "Returns the field ID for a static field of a class, or NULL if the specified static field cannot be found.
Causes an uninitialized class to be initialized.")

  (get-static-object-field object ((class class) (field-id field-id))
    "Returns the content of the static field.")
  (get-static-boolean-field boolean ((class class) (field-id field-id))
    "Returns the content of the static field.")
  (get-static-byte-field byte ((class class) (field-id field-id))
    "Returns the content of the static field.")
  (get-static-char-field char ((class class) (field-id field-id))
    "Returns the content of the static field.")
  (get-static-short-field short ((class class) (field-id field-id))
    "Returns the content of the static field.")
  (get-static-int-field int ((class class) (field-id field-id))
    "Returns the content of the static field.")
  (get-static-long-field long ((class class) (field-id field-id))
    "Returns the content of the static field.")
  (get-static-float-field float ((class class) (field-id field-id))
    "Returns the content of the static field.")
  (get-static-double-field double ((class class) (field-id field-id))
    "Returns the content of the static field.")

  (set-static-object-field :void ((class class) (field-id field-if) (object value))
    "Sets the value of a static field of an object.")
  (set-static-boolean-field :void ((class class) (field-id field-if) (boolean value))
    "Sets the value of a static field of an object.")
  (set-static-byte-field :void ((class class) (field-id field-if) (byte value))
    "Sets the value of a static field of an object.")
  (set-static-char-field :void ((class class) (field-id field-if) (char value))
    "Sets the value of a static field of an object.")
  (set-static-short-field :void ((class class) (field-id field-if) (short value))
    "Sets the value of a static field of an object.")
  (set-static-int-field :void ((class class) (field-id field-if) (int value))
    "Sets the value of a static field of an object.")
  (set-static-long-field :void ((class class) (field-id field-if) (long value))
    "Sets the value of a static field of an object.")
  (set-static-float-field :void ((class class) (field-id field-if) (float value))
    "Sets the value of a static field of an object.")
  (set-static-double-field :void ((class class) (field-id field-if) (double value))
    "Sets the value of a static field of an object.")

  (new-string string (((:pointer char) unicode-chars) (size length))
    "Constructs a new java.lang.String object from an array of Unicode characters.
Returns NULL if the string cannot be constructed.")

  (get-string-length size ((string string))
    "Returns the length (the count of Unicode characters) of a Java string.")
  (get-string-chars (:pointer char) ((string string) ((:return boolean) is-copy))
    "Returns a pointer to the array of Unicode characters of the string.
Returns a second value which indicates whether a copy is made (boolean)")
  (release-string-chars :void ((string string) ((:pointer char) chars))
    "Informs the VM that the native code no longer needs access to chars.
The chars argument is a pointer obtained from string using get-string-chars.")

  (new-string-utf string ((:string string))
    "Constructs a new java.lang.String object from an array of characters in modified UTF-8 encoding.
Returns NULL if the string cannot be constructed.")
  (get-string-utf-length size ((string string))
    "Returns the length in bytes of the modified UTF-8 representation of a string.")
  ;; JNI spec says this returns const byte*, but that's inconsistent
  (get-string-utf-chars (:pointer :char) ((string string) ((:return boolean) is-copy))
    "Returns a pointer to a modified UTF-8 string, or NULL if the operation fails.
Returns a second value which indicates whether a copy is made (boolean*)")
  (release-string-utf-chars :void ((string java-string) ((:pointer :char) char-array))
    "Informs the VM that the native code no longer needs access to the string.
The string argument is a pointer derived from string using get-string-utf-chars.")

  (get-array-length size ((array array))
    "Returns the number of elements in the array.")

  (new-object-array object-array ((size size) (class element-class) (object initial-element))
    "Constructs a new array holding objects in class element-class.
All elements are initially set to initial-element.
Returns NULL if the array cannot be constructed.")
  (get-object-array-element object ((object-array array) (size index))
    "Returns an element of an object array.")
  (set-object-array-element :void ((object-array array) (size index) (object value))
    "Sets an element of an object array.")

  (new-boolean-array boolean-array ((size length))
    "Returns a Java array, or NULL if the array cannot be constructed.")
  (new-byte-array byte-array ((size length))
    "Returns a Java array, or NULL if the array cannot be constructed.")
  (new-char-array char-array ((size length))
    "Returns a Java array, or NULL if the array cannot be constructed.")
  (new-short-array short-array ((size length))
    "Returns a Java array, or NULL if the array cannot be constructed.")
  (new-int-array int-array ((size length))
    "Returns a Java array, or NULL if the array cannot be constructed.")
  (new-long-array long-array ((size length))
    "Returns a Java array, or NULL if the array cannot be constructed.")
  (new-float-array float-array ((size length))
    "Returns a Java array, or NULL if the array cannot be constructed.")
  (new-double-array double-array ((size length))
    "Returns a Java array, or NULL if the array cannot be constructed.")

  (get-boolean-array-elements (:pointer boolean) ((boolean-array array) ((:return boolean) is-copy))
    "Returns a pointer to the array elements, or NULL if the operation fails.
Returns a second value which indicates whether a copy is made (boolean)")
  (get-byte-array-elements (:pointer byte) ((byte-array array) ((:return boolean) is-copy))
    "Returns a pointer to the array elements, or NULL if the operation fails.
Returns a second value which indicates whether a copy is made (boolean)")
  (get-char-array-elements (:pointer char) ((char-array array) ((:return boolean) is-copy))
    "Returns a pointer to the array elements, or NULL if the operation fails.
Returns a second value which indicates whether a copy is made (boolean)")
  (get-short-array-elements (:pointer short) ((short-array array) ((:return boolean) is-copy))
    "Returns a pointer to the array elements, or NULL if the operation fails.
Returns a second value which indicates whether a copy is made (boolean)")
  (get-int-array-elements (:pointer int) ((int-array array) ((:return boolean) is-copy))
    "Returns a pointer to the array elements, or NULL if the operation fails.
Returns a second value which indicates whether a copy is made (boolean)")
  (get-long-array-elements (:pointer long) ((long-array array) ((:return boolean) is-copy))
    "Returns a pointer to the array elements, or NULL if the operation fails.
Returns a second value which indicates whether a copy is made (boolean)")
  (get-float-array-elements (:pointer float) ((float-array array) ((:return boolean) is-copy))
    "Returns a pointer to the array elements, or NULL if the operation fails.
Returns a second value which indicates whether a copy is made (boolean)")
  (get-double-array-elements (:pointer double) ((double-array array) ((:return boolean) is-copy))
    "Returns a pointer to the array elements, or NULL if the operation fails.
Returns a second value which indicates whether a copy is made (boolean)")

  (release-boolean-array-elements :void ((boolean-array array) ((:pointer boolean) elements) (mode mode))
    "Informs the VM that the native code no longer needs access to elements.
The elements argument is a pointer derived from array using get-<type>-array-elements.

The mode argument provides information on how the array buffer should be released.
mode has no effect if elems is not a copy of the elements in array.
  :release -> copy back the content and free the elements buffer;
  :commit  -> copy back the content but do not free the elements buffer;
  :abort   -> free the buffer without copying back the possible changes.")
  (release-byte-array-elements :void ((byte-array array) ((:pointer byte) elements) (mode mode))
    "Informs the VM that the native code no longer needs access to elements.
The elements argument is a pointer derived from array using get-<type>-array-elements.

The mode argument provides information on how the array buffer should be released.
mode has no effect if elems is not a copy of the elements in array.
  :release -> copy back the content and free the elements buffer;
  :commit  -> copy back the content but do not free the elements buffer;
  :abort   -> free the buffer without copying back the possible changes.")
  (release-char-array-elements :void ((char-array array) ((:pointer char) elements) (mode mode))
    "Informs the VM that the native code no longer needs access to elements.
The elements argument is a pointer derived from array using get-<type>-array-elements.

The mode argument provides information on how the array buffer should be released.
mode has no effect if elems is not a copy of the elements in array.
  :release -> copy back the content and free the elements buffer;
  :commit  -> copy back the content but do not free the elements buffer;
  :abort   -> free the buffer without copying back the possible changes.")
  (release-short-array-elements :void ((short-array array) ((:pointer short) elements) (mode mode))
    "Informs the VM that the native code no longer needs access to elements.
The elements argument is a pointer derived from array using get-<type>-array-elements.

The mode argument provides information on how the array buffer should be released.
mode has no effect if elems is not a copy of the elements in array.
  :release -> copy back the content and free the elements buffer;
  :commit  -> copy back the content but do not free the elements buffer;
  :abort   -> free the buffer without copying back the possible changes.")
  (release-int-array-elements :void ((int-array array) ((:pointer int) elements) (mode mode))
    "Informs the VM that the native code no longer needs access to elements.
The elements argument is a pointer derived from array using get-<type>-array-elements.

The mode argument provides information on how the array buffer should be released.
mode has no effect if elems is not a copy of the elements in array.
  :release -> copy back the content and free the elements buffer;
  :commit  -> copy back the content but do not free the elements buffer;
  :abort   -> free the buffer without copying back the possible changes.")
  (release-long-array-elements :void ((long-array array) ((:pointer long) elements) (mode mode))
    "Informs the VM that the native code no longer needs access to elements.
The elements argument is a pointer derived from array using get-<type>-array-elements.

The mode argument provides information on how the array buffer should be released.
mode has no effect if elems is not a copy of the elements in array.
  :release -> copy back the content and free the elements buffer;
  :commit  -> copy back the content but do not free the elements buffer;
  :abort   -> free the buffer without copying back the possible changes.")
  (release-float-array-elements :void ((float-array array) ((:pointer float) elements) (mode mode))
    "Informs the VM that the native code no longer needs access to elements.
The elements argument is a pointer derived from array using get-<type>-array-elements.

The mode argument provides information on how the array buffer should be released.
mode has no effect if elems is not a copy of the elements in array.
  :release -> copy back the content and free the elements buffer;
  :commit  -> copy back the content but do not free the elements buffer;
  :abort   -> free the buffer without copying back the possible changes.")
  (release-double-array-elements :void ((double-array array) ((:pointer double) elements) (mode mode))
    "Informs the VM that the native code no longer needs access to elements.
The elements argument is a pointer derived from array using get-<type>-array-elements.

The mode argument provides information on how the array buffer should be released.
mode has no effect if elems is not a copy of the elements in array.
  :release -> copy back the content and free the elements buffer;
  :commit  -> copy back the content but do not free the elements buffer;
  :abort   -> free the buffer without copying back the possible changes.")

  (get-boolean-array-region :void ((boolean-array array) (size start) (size length) ((:pointer boolean) buffer))
    "Copies a region of a primitive array into a buffer.")
  (get-byte-array-region :void ((byte-array array) (size start) (size length) ((:pointer byte) buffer))
    "Copies a region of a primitive array into a buffer.")
  (get-char-array-region :void ((char-array array) (size start) (size length) ((:pointer char) buffer))
    "Copies a region of a primitive array into a buffer.")
  (get-short-array-region :void ((short-array array) (size start) (size length) ((:pointer short) buffer))
    "Copies a region of a primitive array into a buffer.")
  (get-int-array-region :void ((int-array array) (size start) (size length) ((:pointer int) buffer))
    "Copies a region of a primitive array into a buffer.")
  (get-long-array-region :void ((long-array array) (size start) (size length) ((:pointer long) buffer))
    "Copies a region of a primitive array into a buffer.")
  (get-float-array-region :void ((float-array array) (size start) (size length) ((:pointer float) buffer))
    "Copies a region of a primitive array into a buffer.")
  (get-double-array-region :void ((double-array array) (size start) (size length) ((:pointer double) buffer))
    "Copies a region of a primitive array into a buffer.")
  (set-boolean-array-region :void ((boolean-array array) (size start) (size length) ((:pointer boolean) buffer))
    "Copies back a region of a primitive array from a buffer.")
  (set-byte-array-region :void ((byte-array array) (size start) (size length) ((:pointer byte) buffer))
    "Copies back a region of a primitive array from a buffer.")
  (set-char-array-region :void ((char-array array) (size start) (size length) ((:pointer char) buffer))
    "Copies back a region of a primitive array from a buffer.")
  (set-short-array-region :void ((short-array array) (size start) (size length) ((:pointer short) buffer))
    "Copies back a region of a primitive array from a buffer.")
  (set-int-array-region :void ((int-array array) (size start) (size length) ((:pointer int) buffer))
    "Copies back a region of a primitive array from a buffer.")
  (set-long-array-region :void ((long-array array) (size start) (size length) ((:pointer long) buffer))
    "Copies back a region of a primitive array from a buffer.")
  (set-float-array-region :void ((float-array array) (size start) (size length) ((:pointer float) buffer))
    "Copies back a region of a primitive array from a buffer.")
  (set-double-array-region :void ((double-array array) (size start) (size length) ((:pointer double) buffer))
    "Copies back a region of a primitive array from a buffer.")

  (register-natives code ((class class) ((:pointer (:struct native-method)) native-method) (int number 1))
    "Registers native methods with the class specified by the clazz argument.")
  (unregister-natives code ((class class))
    "Unregisters native methods of a class.
Should not be used in normal native code")

  (monitor-enter code ((object object))
    "Enters the monitor associated with the underlying Java object.
To avoid deadlocks, a monitor entered through a monitor-enter
 must be exited using the monitor-exit, unless the detach-current-thread call is used
 to implicitly release JNI monitors.")
  (monitor-exit code ((object object))
    "The current thread must be the owner of the monitor associated with the underlying Java object")

  (get-vm code ((:return vm))
    "Returns the Java VM interface as a second value.")

  (get-string-region :void ((string string) (size start) (size length) ((:pointer char) buffer))
    "Copies region of Unicode characters to the given buffer.")
  (get-string-utf-region :void ((string string) (size start) (size length) ((:pointer :char) buffer))
    "Translates region of Unicode characters into modified UTF-8 encoding and copies it to the given buffer.")

  (get-primitive-array-critical :pointer ((array array) ((:return boolean) is-copy))
    "See jni spec.")
  (release-primitive-array-critical :void ((array array) (:pointer buffer) (mode mode))
    "See jni spec.")

  (get-string-critical (:pointer char) ((string string) ((:return boolean) is-copy))
    "See jni spec.")
  (release-string-critical :void ((string string) ((:pointer char) buffer))
    "See jni spec.")

  (new-weak-global-reference weak ((object object))
    "Creates a new weak global reference.
Returns NULL if obj refers to null, or if the VM runs out of memory.")
  (delete-weak-global-reference :void ((weak reference))
    "Delete the VM resources needed for the given weak global reference.")

  (exception-check boolean ()
    "Checks for pending exceptions without creating a local reference to the exception object.")

  (new-direct-byte-buffer object ((:pointer adress) (long capacity))
    "Allocates and returns a direct java.nio.ByteBuffer referring to the block of memory
 starting at the memory address address and extending capacity bytes.")
  (get-direct-buffer-address :pointer ((object object))
    "Returns the starting address of the memory region referenced by the buffer.
Returns NULL if the memory region is undefined.")
  (get-direct-buffer-capacity long ((object object))
    "Returns the capacity in bytes of the memory region associated with the buffer.
Returns -1 if the given object is not a direct java.nio.Buffer.")

  (get-object-reference-type reference-type ((object object))
    "Returns the type of the object referred to by the object argument.
The argument object can either be a local, global or weak global reference.")

  (get-module object ((class class))
    "Returns the java.lang.Module object for the module that the class is a member of")

  (is-virtual-thread boolean ((object object))
    "Tests whether an object is a virtual Thread.")

  (get-string-utf-length-as-long long ((string string))
    "Returns the length in bytes of the modified UTF-8 representation of a string."))

;;; Invocation API functions
;;; Exported from native library implementing JVM

(defcfun* (get-default-vm-initargs "JNI_GetDefaultJavaVMInitArgs") code
    (((:pointer (:struct vm-initargs)) default-vm-initargs))
  "Returns a default configuration for the Java VM. Before calling this
function, the VERSION foreign slot of the DEFAULT-VM-INITARGS
parameter must be set to the JNI version that is expected to be
supported. After this function returns, VERSION foreign slot will be
set to the actual JNI version the VM supports.

Returns :OK if the requested version is supported; returns a JNI error
code if the requested version is not supported.")

(defcfun* (get-created-vms "JNI_GetCreatedJavaVMs") code
    (((:pointer vm) return-vms) (size buffer-length) ((:pointer size) return-number))
  "Returns all Java VMs that have been created. Pointers to VMs are
written in the buffer RETURN-VMS buffer in the order they are
created. At most BUFFER-LENGTH number of entries will be written. The
total number of created VMs is returned in RETURN-NUMBER.

Creation of multiple VMs in a single process is not supported.

Returns :OK on success; returns a suitable JNI error code on failure.")

(defcfun* (create-vm "JNI_CreateJavaVM") code
    ((:return return-vm) (:return return-env) ((:pointer (:struct vm-initargs)) vm-initargs))
  "Loads and initializes a Java VM. The current thread is attached to
the Java VM and becomes the main thread. Sets the RETURN-ENV argument
to the JNI environment of the main thread.

Creation of multiple VMs in a single process is not supported.

The second argument to %CREATE-VM is always a pointer to ENV, while
the third argument is a pointer to a VM-INITARGS structure which uses
option strings to encode arbitrary VM start up options.

If IGNORE-UNRECOGNIZED is 1, %CREATE-VM ignores all unrecognized
option strings that begin with '-X' or '_'. If IGNORE-UNRECOGNIZED is
0, %CREATE-VM returns :UNKNOWN-ERROR as soon as it encounters any
unrecognized option strings. All Java VMs must recognize the following
set of standard options:
    -D<name>=<value>         Set a system property
    -verbose[:class|gc|jni]  Enable verbose output. The options can be followed by a comma-separated list of names indicating what kind of messages will be printed by the VM. All nonstandard (VM-specific) names must begin with 'X'.
    vfprintf                 DATA slot is a pointer to the VFPRINTF hook.
    exit                     DATA slot is a pointer to the EXIT hook.
    abort                    DATA slot is a pointer to the ABORT hook.

The module related options, --add-reads, --add-exports, --add-opens,
--add-modules, --limit-modules, --module-path, --patch-module, and
--upgrade-module-path must be passed as option strings using their
'option=value' format instead of their 'option value' format.

In addition, each VM implementation may support its own set of
non-standard option strings. Non-standard option names must begin with
'-X' or '_'.

Returns JNI_OK on success; returns a suitable JNI error code (a negative number) on failure.")
