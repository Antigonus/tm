#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

tm-parked is a tranform, and hence there is a base tape machine.
This transform makes it appear that the head is sitting in void space,
just to the left of leftmost cell on the tape.

(tape tm) holds the base machine

Calling step, #'s, steps into the base machine Calling deallocate, #'d, potentialy
transition to empty.



|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-parked (tape-machine)())

  (defmethod init 
    (
      (tm tm-parked)
      init-list
      &optional 
      (cont-ok (be t))
      (cont-fail (λ()(error 'missing-tm-type)))
      )
    (destructuring-bind
      (&key tm-type base &allow-other-keys) init-list
      (cond
        (base
          (let(
                (base-machine (dup base))
                )
            (setf (HA tm) ∅)
            (cue-leftmost base-machine)
            (setf (tape tm) base-machine) 
            (setf (entanglements tm) (make-entanglements tm))
            (funcall cont-ok)
            ))
        (tm-type
          (let*(
                (type-specifier (if (consp tm-type) (car tm-type) tm-type))
                (options (if (consp tm-type) (cdr tm-type)) ∅)
                (base-machine (make-instance type-specifier))
                )
            (init base-machine (append options init-list)
              (λ()
                (setf (HA tm) ∅)
                (setf (tape tm) base-machine)
                (setf (entanglements tm) (make-entanglements tm))
                (funcall cont-ok)
                )
              cont-fail
              )))
        (t
          (funcall cont-fail)
          ))))

  (defmethod unmount ((tm tm-parked))
    (unmount (tape tm))
    )

  ;; no need to do anything
  ;; don't know if any compilers will freak with an empty body, so I put t
  (defmethod park ((tm tm-parked)) t)


;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod r ((tm tm-parked))
    (declare (ignore tm))
    (error 'parked-access)
    )

  (defmethod w ((tm tm-parked) object)
    (declare (ignore tm object))
    (error 'parked-access)
    )

  (defmethod cue-leftmost ((tm tm-parked))
    (cue-to tm (tape tm)) ; moved (tape tm) to leftmost during init
    )

  ;; steps head from empty space into tape space
  (defmethod s
    (
      (tm tm-parked)
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
      (tm tm-parked)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (a◧ (tape tm) object cont-ok cont-no-alloc)
    )

  (defun d-parked
    (
      tm
      &optional 
      spill
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
      (cont-not-supported (λ()(error 'dealloc-not-supported)))
      (cont-entangled (λ()(error 'dealloc-entangled)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (let(
          (tm-from-the-tape (tape tm))
          )
      (d◧ tm-from-the-tape spill
        (λ(dealloc-object)
          ;; check for collapse to void
          (when (typep tm-from-the-tape 'tm-void) (cue-to tm tm-from-the-tape))
          (funcall cont-ok dealloc-object)
          )
        cont-rightmost
        cont-not-supported
        cont-entangled
        cont-no-alloc
        )))

  (defmethod d
    (
      (tm tm-parked)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
      (cont-not-supported (λ()(error 'dealloc-not-supported)))
      (cont-entangled (λ()(error 'dealloc-entangled)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (d-parked tm spill cont-ok cont-rightmost cont-not-supported cont-entangled cont-no-alloc)
    )

  (defmethod d◧
    (
      (tm tm-parked)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
      (cont-not-supported (λ()(error 'dealloc-not-supported)))
      (cont-entangled (λ()(error 'dealloc-entangled)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (d-parked tm spill cont-ok cont-rightmost cont-not-supported cont-entangled cont-no-alloc)
    )
