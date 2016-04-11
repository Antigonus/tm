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
      (instance tm-list)
      init-list 
      &optional
      (cont-ok (be t))
      (cont-fail (λ()(error 'bad-init-value)))
      )
    (destructuring-bind
      (&key tape-space mount &allow-other-keys) init-list

      ;; tape-space shouldn't be defined for tm-list, but we let it slide if the 
      ;; value it is set to is set tm-list.  tape-space is for tm-void and tm-singular.
      (when
        (∧ tape-space (¬ (eq tape-space 'tm-list)))
        (return-from init (funcall cont-fail))
        )
      (unless mount
        (change-class instance 'tm-void)
        (return-from init (init tm init-list cont-ok cont-fail))
        )
      (unless (cdr mount)
        (change-class instance 'tm-singular)
        (return-from init (init tm init-list cont-ok cont-fail))
        )
      (setf (tape instance) mount)
      (setf (HA instance) mount)
      (funcall cont-ok)
      ))

  (defmethod mount ((sequence cons) &optional (cont-ok #'echo) cont-fail)
    (declare (ignore cont-fail))
    (let(
          (instance (make-instance 'tm-list))
          )
      (init 'tm-list :mount sequence)
      (funcall cont-ok instance)
      ))
