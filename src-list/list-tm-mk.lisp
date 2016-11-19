#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list-tm machines. Typically the tm will be a list-nd-tm or a list-solo-tm
  object, and then this gets called due to the inheritance structure.
  

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; making list machines from other objects
;;
  (defmethod init 
    (
      (tm list-tm)
      (init-value cons)
      &optional
      (cont-ok #'echo)
      (cont-fail (λ()(error 'bad-init-value)))
      &rest ⋯
      )
    (declare (ignore ⋯ cont-fail))
    (setf (HA tm) init-value)
    (setf (tape tm) init-value)
    (funcall cont-ok tm)
    )
    

