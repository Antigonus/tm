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
      (cond
        ((¬ mount)
          (change-class tm 'tm-void)
          (setf (HA tm) 'tm-list)
          (setf (tape tm) ∅)
          (setf (parameters tm) ∅)
          (setf (entanglements tm) (make-entanglements tm))
          (funcall cont-ok)
          )
        ((consp mount) 
          (setf (HA tm) mount)
          (setf (tape tm) mount)
          (setf (parameters tm) ∅)
          (setf (entanglements tm) (make-entanglements tm))
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
