#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

(HA tm) holds the type of the tape space.
(tape tm) holds a tape machine

This machine is typically created by allocating a cell to a tm-parked-singular machine.

Calling step, #'s, steps into the tape space.
Calling deallocate, #'d, potentialy transition to 'tm-parked-singular.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;

  (defclass tm-parked-tape (tm-void)())

  (defmethod init 
    (
      (tm tm-parked-tape)
      init-list
      &optional 
      (cont-ok (be t))
      (cont-fail (error 'bad-init-value))
      )
    (destructuring-bind
      (&key tape-space mount &allow-other-keys) init-list
      (unless 
        (∧
          tape-space
          mount
          (≥ (length mount) 2)
          )
        (return-from init (funcall cont-fail))
        )
      (setf (HA tm) tape-space)
      (setf (tape tm) (mk tape-space init-list))
      (funcall cont-ok)
      ))


;;--------------------------------------------------------------------------------
;; primitive methods
;;
  ;; steps head from void space into tape space
  (defmethod s
    (
      (tm tm-parked-tape)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (declare (ignore cont-rightmost))
    (cue-to tm (tape tm))
    (funcall cont-ok)
    )

  (defmethod a
    (
      (tm tm-parked-tape)
      object
      &optional
      cont-ok
      (cont-no-alloc (error 'tm-alloc-fail))
      )
    (a◧ (tape tm) object cont-ok cont-no-alloc)
    )

  (defmethod d 
    (
      (tm tm-parked-tape)
      &optional 
      spill
      cont-ok
      (cont-no-dealloc (λ()(error 'dealloc-fail)))
      (cont-no-alloc  (λ()(error 'alloc-fail)))
      )
    (let(
          (tm-tape-space (tape tm)) ; has at least two objects now, could collapse to singular
          )
      (d◧ tm-tape-space spill
        (λ()
          (when (typep tm-tape-space 'tm-singular) (cue-to tm tm-tape-space))
          (funcall cont-ok)
          )
        cont-no-dealloc 
        cont-no-alloc
        )))
