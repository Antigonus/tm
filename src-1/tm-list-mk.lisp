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
      (&key tm-type mount &allow-other-keys) init-list

      ;; tm-type shouldn't be defined for tm-list, but we let it slide if the 
      ;; value it is set to is set tm-list.  tm-type is for tm-void and tm-singular.
      (when
        (∧ tm-type (¬ (eq tm-type 'tm-list)))
        (return-from init 
          (funcall cont-fail)
          ))
      (unless mount
        (change-class tm 'tm-void)
        (return-from init 
          (init tm {:tm-type 'tm-list} cont-ok cont-fail)
          ))
      (unless (cdr mount)
        (change-class tm 'tm-singular)
        (return-from init 
          (init tm {:tm-type 'tm-list :mount (car mount)} cont-ok cont-fail)
          ))
      (setf (tape tm) mount)
      (setf (HA tm) mount)
      (funcall cont-ok)
      ))

  (defmethod mount ((sequence cons) &optional (cont-ok #'echo) cont-fail)
    (declare (ignore cont-fail))
    (let(
          (instance (make-instance 'tm-list))
          )
      (init instance {:mount sequence}
        (λ()(funcall cont-ok instance))
        (be ∅)
        )))
