#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list machines from other objects.
  Make other objects from list machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-list (tape-machine)())

;;--------------------------------------------------------------------------------
;; making tm-list machines from other objects
;;
  (defmethod init 
    (
      (tm tm-list)
      init-list 
      &optional
      (cont-ok (be t))
      (cont-fail (λ()(error 'bad-init-value)))
      )
    (destructuring-bind
      (&key mount &allow-other-keys) init-list
      (cond
        ((¬ mount)
          (change-class tm 'tm-void)
          (init tm {:tm-type 'tm-list} cont-ok cont-fail)
          )
        ((consp mount) 
          (setf (tape tm) mount)
          (setf (HA tm) mount)
          (funcall cont-ok)
          )
        (t
          (funcall cont-fail)
          ))))

  (defmethod mount ((sequence cons) &optional (cont-ok #'echo) cont-fail)
    (declare (ignore cont-fail))
    (let(
          (instance (make-instance 'tm-list))
          )
      (init instance {:mount sequence}
        (λ()(funcall cont-ok instance))
        (be ∅)
        )))
