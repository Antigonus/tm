#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  The tape machine version of the bit bucket ...

  A void affine machine, but reading always returns ∅.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-void-affine ()())

  (defmethod tm-init ((tm tm-void-affine) init-list)
    (cond
      ((¬ init-list) 
        (setf (HA tm) 'tm-void-affine)
        tm
        )
      (t 
        (error 'tm-mk-bad-init-type)
        )
      ))


;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod r ((tm tm-void-affine))
    (declare (ignore tm))
    ∅
    )
  (defmethod w ((tm tm-void-affine) object)
    (declare (ignore tm object))
    t
    )

  (defmethod cue-leftmost  ((tm tm-void-affine)) 
    tm
    )

  (defun heads-on-same-cell-void-affine-0 (tm0 tm1 cont-true cont-false)
    (if
      (∧
        (typep tm0 'tm-void-affine)
        (typep tm1 'tm-void-affine)
        (eq (HA tm0) (HA tm1))
        )
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defmethod heads-on-same-cell 
    (
      (tm0 tm-void-affine) 
      (tm1 tape-machine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-void-affine-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod heads-on-same-cell 
    (
      (tm0 tape-machine) 
      (tm1 tm-void-affine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-void-affine-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod s
    (
      (tm tm-void-affine)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      (cont-mount-failed (λ()(error 'tm-mount-failed)))
      )
    (declare (ignore cont-rightmost cont-mount-failed))
    (funcall cont-ok)
    )

  ;; allocate a new window cell, and writes it.
  ;; note this routine has been optimized and the allocation part is gone ;-)
  (defmethod a 
    (
      (tm tm-void-affine) 
      object 
      &optional
      (cont-ok (be t))
      (cont-no-alloc (be ∅))
      )
    (declare (ignore cont-no-alloc)) ; we have an infinite supply of window cells ;-)
    (w tm object)
    (funcall cont-ok)
    )

  (defun d-void-affine
    (
      tm 
      spill
      cont-ok
      cont-rightmost
      cont-no-alloc
      )
    (declare (ignore cont-rightmost cont-no-alloc))
    (when spill (as spill (HA tm)))
    (funcall cont-ok (HA tm))
    )

  (defmethod d 
    (
      (tm tm-void-affine)
      &optional 
      spill
      (cont-ok (be t))
      cont-rightmost
      cont-no-alloc
      )
    (d-void-affine tm spill cont-ok cont-rightmost cont-no-alloc)
    )
