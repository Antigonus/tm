#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list-tm machines. Typically the tm will be a list-nd-tm or a list-solo-tm
  instance, and then this gets called due to the inheritance structure.
  

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; making list machines from other instances
;;
  (defun-typed init 
    (
      (tm list-tm)
      (init-value cons)
      &optional
      (cont-ok #'echo)
      (cont-fail (λ()(error 'bad-init-value)))
      &rest ⋯
      )
    (declare (ignore ⋯ cont-fail))
    (setf (head tm) init-value)
    (setf (tape tm) init-value)
    [cont-ok tm]
    )
    

