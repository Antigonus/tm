#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

tm-parked-tape is a tranform, and hence there is a base tape machine.
This transform makes it appear that the head is sitting in empty space,
but just to the left of leftmost.

(tape tm) holds the base tape machine

Calling step, #'s, steps into the base machine
Calling deallocate, #'d, potentialy transition to empty.

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
            (setf (tape tm) (dup base)) ; wonder if we should cue leftmost ...
            )
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
  ;; steps head from empty space into tape space
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
    (let(
          (tm-from-the-tape (tape tm))
          )
      (d◧ tm-from-the-tape spill
        (λ(dealloc-object)
          (if 
            (typep tm-from-the-tape 'tm-void)
            (progn
              (cue-to tm tm-from-the-tape)
              (funcall cont-ok dealloc-object)
              )))
        cont-no-dealloc
        cont-no-alloc
        )))

  (defmethod d◧
    (
      (tm tm-parked-tape)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-no-dealloc (λ()(error 'dealloc-fail)))
      (cont-no-alloc  (λ()(error 'alloc-fail)))
      )
    (declare (ignore tm spill cont-ok cont-no-alloc))
    (funcall cont-no-dealloc)
    )
