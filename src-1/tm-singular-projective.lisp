#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  The cell type for this machine is unique. It is a singular-projective cell type.
  There are no generators for cells of this type.

  The tape on this machine is intialized with exactly one such cell, a single cell.  Hence
  it has a single cell of a unique type. 

  It is illegal to deallocate the cell under the head, and as this machine has one cell,
  and the head is always on top of it, no cell can ever be deallocated.  As it can not
  be deallocated it can not be shared or gathered.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-singular-projective (tape-machine)())

  (defmethod tm-init
    (
      (tm tm-singular-projective)
      &optional
      init 
      (cont-ok #'echo) 
      cont-fail
      )
    (declare (ignore cont-fail))
    (if
      init
      (setf (HA tm) init)
      (setf (HA tm) 'tm-singular-projective)
      )
    (funcall cont-ok tm)
    )


;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod r ((tm tm-singular-projective)) (HA tm))


  (defmethod w ((tm tm-singular-projective) object)
    (setf (HA tm) object)
    t
    )
 
  ;; already on leftmost
  (defmethod cue-leftmost  ((tm tm-singular-projective)) 
    tm
    )

  (defun heads-on-same-cell-singular-projective-0 (tm0 tm1 cont-true cont-false)
    (if
      (∧
        (typep tm0 'tm-singular-projective)
        (typep tm1 'tm-singular-projective)
        (eq (HA tm0) (HA tm1))
        )
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defmethod heads-on-same-cell 
    (
      (tm0 tm-singular-projective) 
      (tm1 tape-machine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-singular-projective-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod heads-on-same-cell 
    (
      (tm0 tape-machine) 
      (tm1 tm-singular-projective) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-singular-projective-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod s
    (
      (tm tm-singular-projective)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (declare (ignore cont-ok))
    (funcall cont-rightmost)
    )

  ;; allocate a cell .. but can't
  (defmethod a
    (
      (tm tm-singular-projective)
      object
      &optional
      cont-ok
      (cont-no-alloc (error 'tm-alloc-fail))
      )
    (declare (ignore tm object cont-ok))
    (funcall cont-no-alloc)
    )

  (defmethod d 
    (
      (tm tm-singular-projective)
      &optional 
      spill
      cont-ok
      (cont-rightmost (λ()(error 'tm-deallocation-request-at-rightmost)))
      cont-no-alloc
      )
    (declare (ignore tm spill cont-ok cont-no-alloc))
    (funcall cont-rightmost)
    )
