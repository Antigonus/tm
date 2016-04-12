#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

(HA tm) holds the type of the tape space
(tape tm) holds an object

Calling alloc, #'a, transitions into the tape space.
Calling dealloc, #'d transitions to 'tm-void.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-singular (tm-void)())

  (defmethod init 
    (
      (tm tm-singular)
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
        (setf (HA tm) 'tm-singular)
        )
      (setf (tape tm) mount)
      (funcall cont-ok)
      ))

  (defmethod unmount ((tm tm-singular))
    (case (HA tm)
      (tm-list {(tape tm)})
      (tm-array #((tape tm)))
      (t (error 'can-not-unmount))
      ))

;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod r ((tm tm-singular)) (tape tm))
  (defmethod w ((tm tm-singular) object) (setf (tape tm) object))

  ;; transitions into the tape space
  (defmethod a
    (
      (tm tm-singular)
      object-1
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    (let(
          (tm-type (HA tm))
          (object-0 (tape tm))
          )
      (change-class tm tm-type)
      (init tm {:tm-type tm-type :mount {object-0 object-1}} cont-ok cont-no-alloc)
      ))

  (defmethod d 
    (
      (tm tm-singular)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-no-dealloc (λ()(error 'dealloc-fail)))
      (cont-no-alloc  (λ()(error 'alloc-fail)))
      )
    (declare (ignore tm spill cont-ok cont-no-alloc))
    (funcall cont-no-dealloc) ; because the head is on rightmost
    )

  (defmethod d◧
    (
      (tm tm-singular)
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
        (as spill dealloc-object #'do-nothing (λ()(return-from d◧ (funcall cont-no-alloc))))
        )
      (change-class tm 'tm-void)
      (init tm {:tm-type tm-type})
      (funcall cont-ok dealloc-object)
      ))

