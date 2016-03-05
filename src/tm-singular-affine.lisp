#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  This tape machine has a single cell on a circular tape. The tape
  has no rightmost and no leftmost.

  =

  The cells on this machine's tape are special.  Though there may be many cells, the
  object held in every one of them is the one and same object.  Hence a write through any
  cell, affects the one single object. We will call this kind of cell a 'window cell'.

  Any new cells allocated to the tape are also these same special cells. This should not
  be surprising as most implementations are like this, i.e. they use only one kind of
  cell.

  Initially the tape has a countably infinite number of window cells, so deallocating or
  allocating a few of them won't have any affect.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-singular-affine (tape-machine)())

  (defun mk-tm-singular-affine (&optional init (cont-ok #'echo) cont-fail)
    (declare (ignore cont-fail))
    (let(
          (tm (make-instance 'tm-singular-affine))
          )
      (if
        init
        (setf (HA tm) init)
        (setf (HA tm) 'tm-singular-affine)
        )
      (funcall cont-ok tm)
      ))

  (mk-tm-hook 'tm-singular-affine #'mk-tm-singular-affine)

;;--------------------------------------------------------------------------------
;; essential methods
;;
  (defmethod r ((tm tm-singular-affine)) (HA tm))


  (defmethod w ((tm tm-singular-affine) object)
    (setf (HA tm) object)
    t
    )


  (defmethod cue-leftmost  ((tm tm-singular-affine)) 
    (declare (ignore tm))
    t
    )

  (defun heads-on-same-cell-singular-affine-0 (tm0 tm1 cont-true cont-false)
    (if
      (∧
        (typep tm0 'tm-singular-affine)
        (typep tm1 'tm-singular-affine)
        (eq (HA tm0) (HA tm1))
        )
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defmethod heads-on-same-cell 
    (
      (tm0 tm-singular-affine) 
      (tm1 tape-machine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-singular-affine-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod heads-on-same-cell 
    (
      (tm0 tape-machine) 
      (tm1 tm-singular-affine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-singular-affine-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod s
    (
      (tm tm-singular-affine)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (declare (ignore cont-rightmost))
    (funcall cont-ok)
    )

  ;; allocate a new window cell, and writes it.
  ;; note this routine has been optimized and the allocation part is gone ;-)
  (defmethod a 
    (
      (tm tm-singular-affine) 
      object 
      &optional
      (cont-ok (be t))
      (cont-no-alloc (be ∅))
      )
    (declare (ignore cont-no-alloc)) ; we have an infinite supply of window cells ;-)
    (w tm object)
    (funcall cont-ok)
    )

  (defun d-singular-affine
    (
      tm 
      spill
      cont-ok
      cont-rightmost
      cont-no-alloc
      )
    (declare (ignore cont-rightmost cont-no-alloc))
    (when spill (as spill (HA tm)))
    (funcall cont-ok)
    )

  (defmethod d 
    (
      (tm tm-singular-affine)
      &optional 
      spill
      (cont-ok (be t))
      cont-rightmost
      cont-no-alloc
      )
    (d-singular-affine tm spill cont-ok cont-rightmost cont-no-alloc)
    )

  (defmethod ◧d 
    (
      (tm tm-singular-affine)
      &optional 
      spill
      (cont-ok (be t))
      cont-rightmost
      cont-no-alloc
      )
    (d-singular-affine tm spill cont-ok cont-rightmost cont-no-alloc)
    )

