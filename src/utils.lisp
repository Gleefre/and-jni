(in-package #:and-jni/utils)

(declaim (inline ensure-car))
(defun ensure-car (x)
  (if (consp x) (car x) x))

(declaim (inline ensure-list))
(defun ensure-list (x)
  (if (listp x) x (list x)))
