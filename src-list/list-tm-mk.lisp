#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; making list machines from other objects
;;
  (defmethod init 
    (
      (tm list-tm)
      init-list 
      &optional
      (cont-ok (be t))
      (cont-fail (λ()(error 'bad-init-value)))
      &rest ⋯
      )
    (declare (ignore ⋯))
    (destructuring-bind
      (&key mount &allow-other-keys) init-list
      (cond
        ((∧ mount (consp mount))
          (setf (HA tm) mount)
          (setf (tape tm) mount)
          (funcall cont-ok)
          )
        (t
          (funcall cont-fail)
          ))))
    
