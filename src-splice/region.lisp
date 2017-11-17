#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Every tape machine partitions the tape into a lefthand side and a right hand side.  The
tape machine itself is on the rightmost cell of the lefthand side.

When there are two tape machines, the overlapping right hand side from the first, and left
hand side from the second, form a 'region'.

If during a deletion operation a tape machine is found to have its head on a cell to be
deleted, the deletion fails and we take a collision continuation.  It is different for
regions.  If a region boundary is deleted, the boundary is simply moved to the nearest
place where it still bounds the region.  Even empty regions continue to have a location,
though that location may move.

Thus we want region machines to have their own type, so that the deletion code can handle
them differently. We will also need to know which pairs go together.

A region is a tape machine with an artificial right bound. One can think of this
artificial right bound as a second head on the machine. The left bound is exclusive. The
right bound is inclusive.

Entangling with a region returns another region with bounds on the same cells.

A region is 'region-empty type iff the tape is empty.  If the machine is parked, then
the first cell in the region is leftmost.  If the left bound address is the same as the
right bound address, then the region is empty.  Note it still has a position.

|#

;;--------------------------------------------------------------------------------
;; type
;;
  ;; here we inherit and add a slot of the right bound
  (def-type region (tm) ())

  (def-type region-abandoned (region abondoned))
  (def-type region-empty     (region empty))
  (def-type region-parked    (region parked))
  (def-type region-active    (region active))

  (defun-typed to-abandoned ((r region))(change-class r 'region-abandoned))
  (defun-typed to-empty     ((r region))(change-class r 'region-empty))
  (defun-typed to-parked    ((r region))(change-class r 'region-parked))
  (defun-typed to-active    ((r region))(change-class r 'region-active))

;;--------------------------------------------------------------------------------
;; absolue head control
;;
  (def-function-class u-right (tm address &optional ➜))

  (defun-typed u-right ((tm tm-empty) address &optional ➜)
    (destructuring-bind
      (
        &key
        (➜bound (be ∅)) ; takes one operand, the spill amount
        &allow-other-keys
        )
      ➜
      [➜bound address]
      ))

  (def-function-class @-right (tm &optional ➜))
  (defun-typed @-right ((tm tm-abandoned) &optional ➜)
    (declare (ignore tm ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed @-right ((tm tm-empty) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (
        &key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
