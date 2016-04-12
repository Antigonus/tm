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
      (cont-fail (λ()(error 'missing-tm-type)))
      )
    (destructuring-bind
      (&key tm-type &allow-other-keys) init-list
      (if tm-type
        (progn
          (let(
                (tape-space (make-instance tm-type))
                )
            (init tape-space init-list
              (λ()
                (setf (HA tm) tm-type)
                (setf (tape tm) tape-space)
                (funcall cont-ok)
                )
              cont-fail
              )))
        (funcall cont-fail)
        )))



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
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    (a◧ (tape tm) object cont-ok cont-no-alloc)
    )

  (defmethod d 
    (
      (tm tm-parked-tape)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-no-dealloc (λ()(error 'dealloc-fail)))
      (cont-no-alloc  (λ()(error 'alloc-fail)))
      )
    (let*(
          (tm-on-tape (tape tm)) ; has at least two objects now, could collapse to singular
          (dealloc-object (r tm-on-tape))
          )
      (when spill 
        (as spill dealloc-object #'do-nothing (λ()(return-from d (funcall cont-no-alloc))))
        )
      (cond
        ((doubleton tm-on-tape)
          (s tm-on-tape
            (λ() 
              (let(
                    (tm-type (HA tm))
                    (keep-object (r tm-on-tape))
                    )
                (change-class tm 'tm-parked-singular)
                (init tm {:tm-type tm-type :mount keep-object}
                  (λ()(funcall cont-ok dealloc-object))
                  (λ()(error 'impossible-to-get-here))
                  )))
            (λ()(error 'impossible-to-get-here))
            ))
        (t
          (d◧ tm-on-tape ∅
            #'echo
            cont-no-dealloc 
            (λ()(error 'impossible-to-get-here))
            )))))
