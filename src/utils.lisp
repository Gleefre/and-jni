(in-package #:and-jni/utils)

(declaim (inline ensure-car))
(defun ensure-car (x)
  (if (consp x) (car x) x))
