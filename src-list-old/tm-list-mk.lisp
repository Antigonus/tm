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
  (defclass tm-list (tape-machine)())

;;--------------------------------------------------------------------------------
;; making tm-list machines from other objects
;;
  (defun make-entanglements (tm-first)
    (let(
          (entanglements (make-instance 'tm-list))
          (first-cell (cons tm-first ∅))
          )
      (setf (state entanglements) active)
      (setf (HA entanglements) first-cell)
      (setf (tape entanglements) first-cell)
      (setf (parameters entanglements) ∅)
      (setf (entanglements entanglements) ∅)
      entanglements
      ))

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
      (setf (entanglements tm) (make-entanglements tm))
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
    
  (defmethod mount
    (
      (sequence cons)
      &optional
      (cont-ok #'echo) 
      (cont-fail (λ()(error 'mount-failed)))
      )
    (let(
          (instance (make-instance 'tm-list))
          )
      (init instance {:mount sequence}
        (λ()(funcall cont-ok instance))
        cont-fail
        )))
