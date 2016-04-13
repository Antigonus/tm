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
      (cont-fail (λ()(error 'bad-init-value :text "A mount value must be given.")))
      )
    (destructuring-bind
      (&key tm-type (mount ∅ mountp) &allow-other-keys) init-list
      (unless mountp (funcall cont-fail))
      (if tm-type
        (setf (HA tm) tm-type)
        (setf (HA tm) 'tm-parked-singular)
        )
      (setf (tape tm) mount)
      (funcall cont-ok)
      ))

  (defmethod unmount ((tm tm-parked-singular))
    (case (HA tm)
      (tm-list {(tape tm)})
      (tm-array #((tape tm)))
      (t (error 'can-not-unmount))
      ))

  ;; no need to do anything
  ;; don't know if any compilers will freak with an empty body, so I put t
  (defmethod park ((tm tm-parked-singular)) t)


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
    (init tm {:tm-type (HA tm) :mount (tape tm)}
      cont-ok
      (λ()(error 'unrecognized-instance-type))
      ))

  (defmethod a
    (
      (tm tm-parked-singular)
      object-1
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    (let(
          (tm-type (HA tm))
          (object-0 (tape tm))
          )
      (change-class tm 'tm-parked-tape)
      (init tm {:tm-type tm-type :mount {object-1 object-0}} cont-ok cont-no-alloc)
      ))

  (defmethod d 
    (
      (tm tm-parked-singular)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-no-dealloc (λ()(error 'dealloc-fail)))
      (cont-no-alloc  (λ()(error 'alloc-fail)))
      )
    (declare (ignore cont-no-dealloc))
    (let(
          (tm-type (HA tm))
          (dealloc-object (tape tm))
          )
      (when spill 
        (as spill dealloc-object #'do-nothing (λ()(return-from d (funcall cont-no-alloc))))
        )
      (change-class tm 'tm-void)
      (init tm {:tm-type tm-type}
        (λ()(funcall cont-ok dealloc-object))
        (λ()(error 'impossible-to-get-here))
        )))

