#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

(HA tm) holds the type of the tape space.
(tape tm) holds a tape machine

This machine is typically created by allocating a cell to a tm-parked-singular machine.

Calling step, #'s, steps into the tape space.
Calling deallocate, #'d, potentialy transition to 'tm-parked-singular.

.. need to use typeof everywhere on base, and not use (HA) as for void
and singular, as otherwise we are just setting ourselves up for mismatch
hazards.

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
      (&key tm-type base &allow-other-keys) init-list
      (cond
        (base
          (setf (HA tm) ∅)
          (setf (tape tm) base)
          )
        (tm-type
          (let(
                (tape-space (make-instance tm-type))
                )
            (init tape-space init-list
              (λ()
                (setf (HA tm) ∅)
                (setf (tape tm) tape-space)
                (funcall cont-ok)
                )
              cont-fail
              )))
        (t
          (funcall cont-fail)
          ))))

  (defmethod unmount ((tm tm-parked-tape))
    (unmount (tape tm))
    )

  ;; no need to do anything
  ;; don't know if any compilers will freak with an empty body, so I put t
  (defmethod park ((tm tm-parked-tape)) t)


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
          (tm-from-the-tape (tape tm)) ; has at least two objects now, could collapse to singular
          (dealloc-object (r tm-from-the-tape))
          )
      (when spill 
        (as spill dealloc-object #'do-nothing (λ()(return-from d (funcall cont-no-alloc))))
        )
      (cond
        ((doubleton tm-from-the-tape)
          (s tm-from-the-tape
            (λ() 
              (let(
                    (tm-type (type-of tm-from-the-tape))
                    (keep-object (r tm-from-the-tape))
                    )
                (change-class tm 'tm-parked-singular)
                (init tm {:tm-type tm-type :mount keep-object}
                  (λ()(funcall cont-ok dealloc-object))
                  (λ()(error 'impossible-to-get-here))
                  )))
            (λ()(error 'impossible-to-get-here))
            ))
        (t
          (d◧ tm-from-the-tape ∅
            #'echo
            cont-no-dealloc 
            (λ()(error 'impossible-to-get-here))
            )))))
