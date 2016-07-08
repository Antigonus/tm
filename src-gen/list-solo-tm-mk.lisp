#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list machines.

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass solo-tm-list (solo-tape-machine)())

;;--------------------------------------------------------------------------------
;; making tm-list machines from other objects
;;
  (defmethod init 
    (
      (tm solo-tm-list)
      init-list 
      &optional
      (cont-ok (be t))
      (cont-fail (λ()(error 'bad-init-value)))
      )
    (destructuring-bind
      (&key mount &allow-other-keys) init-list
      (setf (parameters tm) ∅)
      (cond
        ((¬ mount)
          (setf (state tm) void)
          (setf (HA tm) ∅)
          (setf (tape tm) ∅)
          (funcall cont-ok)
          )
        ((consp mount) 
          (setf (state tm) active)
          (setf (HA tm) mount)
          (setf (tape tm) mount)
          (funcall cont-ok)
          )
        (t
          (funcall cont-fail)
          ))))
    
