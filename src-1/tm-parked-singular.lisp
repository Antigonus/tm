#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

(HA tm) holds the type of the tape space.
(tape tm) holds a single object.

This machine is typically created by allocating a cell to a tm-void machine.

Calling alloc, #'a, transition to 'tm-parked-tape.
Calling step, #'s, transtion to 'tm-singular.
Calling deallocate, #'d, transition to 'tm-void.


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;

  (defclass tm-parked-singular (tm-void)())

  (defmethod init 
    (
      (tm tm-parked-singular)
      init-list
      &optional 
      (cont-ok (be t))
      (cont-fail (error 'bad-init-value))
      )
    (destructuring-bind
      (&key tape-space mount &allow-other-keys) init-list
      (when (∧ mount (cdr mount)) 
        (return-from init (funcall cont-fail))
        )
      (when mount 
        (setf (tape tm) (car mount))
        )
      (if tape-space
        (setf (HA tm) tape-space)
        (setf (HA tm) 'tm-parked-singular)
        )
      (funcall cont-ok)
      ))


;;--------------------------------------------------------------------------------
;; primitive methods
;;
  ;; steps head from void space into tm-singular space
  (defmethod s
    (
      (tm tm-parked-singular)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (declare (ignore cont-rightmost))
    (change-class tm 'tm-singular)
    (funcall cont-ok)
    )

  (defmethod a
    (
      (tm tm-parked-singular)
      object-1
      &optional
      (cont-ok (be t))
      (cont-no-alloc (error 'tm-alloc-fail))
      )
    (let(
          (tm1 (dup tm))
          (object-0 (tape tm))
          )
      (change-class tm1 (HA tm))
      (init tm1 :mount {object-0 object-1}
        (λ()
          (change-class tm 'tm-parked-tape)
          (setf (tape tm) (tape tm1))
          (funcall cont-ok)
          )
        cont-no-alloc
        )))

  (defmethod d 
    (
      (tm tm-parked-singular)
      &optional 
      spill
      cont-ok
      (cont-no-dealloc (λ()(error 'dealloc-fail)))
      (cont-no-alloc  (λ()(error 'alloc-fail)))
      )
    (declare (ignore cont-no-dealloc))
    (when spill 
      (as spill (tape tm) #'do-nothing (λ()(return-from d (funcall cont-no-alloc))))
      )
    (setf (tape tm) ∅)
    (change-class tm 'tm-void)
    (funcall cont-ok)
    )

