(in-package #:and-jni/parse-args)

;; Returns the following values:
;;   lambda-list  -- lambda list for the function/macro
;;   call-args    -- call arguments for foreign-funcall-pointer
;;   returns-spec -- foreign objects spec for with-foreign-objects
;;   returns-read -- foreign objects read forms to return with values
;;   macro        -- t if the macro should be defined
(defun parse-args (args)
  (loop with macro = (eq '&rest (caar (last args)))
        with rest = nil
        with optional = nil
        for (type argname . default-value) in args

        if rest
        do (error "Found arguments after &rest. ~S"
                  (list* type argname default-value))
        else when (eq type '&rest)
        collect '&rest into lambda-list and
        do (setf rest t)

        if (and default-value rest)
        do (error "Default value specified for the &rest argument. ~S"
                  (list* type argname default-value))
        else if (and default-value (not optional))
        collect '&optional into lambda-list and
        do (setf optional t)
        else when (and optional (not rest) (not default-value))
        do (warn "No default value found for an argument after &optional. ~S"
                 (list* type argname default-value))

        if (eq (u:ensure-car type) :return)
        collect `(,argname ',(or (cadr (u:ensure-list type)) :pointer)) into returns-spec and
        collect `(cffi:mem-aref ,argname ',(or (cadr (u:ensure-list type)) :pointer)) into returns-read and
        collect :pointer into call-args and
        collect (if macro `',argname argname) into call-args
        else
        collect (if (and optional (not rest))
                    `(,argname ,@default-value)
                    argname)
        into lambda-list and
        unless rest
        collect (if macro `',type type) into call-args
        end and
        collect argname into call-args

        finally (when macro
                  (push 'list* call-args))
                (return (values lambda-list call-args returns-spec returns-read macro))))
