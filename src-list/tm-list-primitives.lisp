#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Tape is implemented with a singly linked list.

  When a machine is first created it will be of type empty projective, Upon 
  the allocation of a new cell, it then becomes a tm-list.

  Deallocation, #'d◧  of the last cell will cause tm-list to collapse back
  into projective machine.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; accessing data
;;
  ;; void and parked states handled in tm-primitive
  (defmethod r-0 ((tm tm-list) (state active) cont-ok cont-parked)
    (declare (ignore state cont-parked))
    (funcall cont-ok (car (HA tm)))
    )

  ;; void and parked states handled in tm-primitive
  (defmethod w-0 ((tm tm-list) (state active) object cont-ok cont-parked)
    (declare (ignore state cont-parked))
    (setf (car (HA tm)) object)
    (funcall cont-ok)
    )

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  ;; our tape is never nil, so this returns true
  ;; void state handled in tm-primitive
  (defmethod cue-leftmost-0  ((tm tm-list) (state active) cont-ok cont-void)
    (declare (ignore state cont-void))
    (setf (HA tm) (tape tm))
    (funcall cont-ok)
    )
  (defmethod cue-leftmost-0  ((tm tm-list) (state parked) cont-ok cont-void)
    (declare (ignore cont-void))
    (setf (state tm) active)
    (setf (HA tm) (tape tm))
    (funcall cont-ok)
    )
  
;;--------------------------------------------------------------------------------
;;  head location predicates
;;
  ;; void parked states handled in tm-primitives
  (defmethod heads-on-same-cell-0
    (
      (tm0 tm-list) 
      (state0 active)
      (tm1 tm-list) 
      (state1 active)
      cont-true
      cont-false
      cont-parked 
      ) 
    (declare (ignore state0 state1 cont-parked))
    (if
      (eq (HA tm0) (HA tm1))
      (funcall cont-true)
      (funcall cont-false)
      ))

;;--------------------------------------------------------------------------------
;; head stepping
;;
  ;; void parked states handled in tm-primitives
  (defmethod s-0
    (
      (tm tm-list)
      (state active)
      cont-ok
      cont-rightmost
      )
    (if 
      (cdr (HA tm))
      (progn
        (setf (HA tm) (cdr (HA tm)))
        (funcall cont-ok)
        )
      (funcall cont-rightmost)
      ))


;;--------------------------------------------------------------------------------
;; cell allocation
;;
  ;; void state handled in tm-primitives
  (defmethod a◧-0
    (
      (tm tm-list)
      (state active)
      object 
      cont-ok
      cont-not-supported
      cont-no-alloc
      )
    (declare (ignore state cont-not-supported cont-no-alloc)) ; should do something with cont-no-alloc
    (setf (tape tm) (cons object (tape tm)))
    (funcall cont-ok)
    )
  (defmethod a◧-0 ; identical to above, for parked
    (
      (tm tm-list)
      (state parked)
      object 
      cont-ok
      cont-not-supported
      cont-no-alloc
      )
    (declare (ignore state cont-not-supported cont-no-alloc)) ; should do something with cont-no-alloc
    (setf (tape tm) (cons object (tape tm)))
    (funcall cont-ok)
    )

  ;; Allocates a cell to the right of the head.
  (defmethod a-0
    (
      (tm tm-list)
      (state active)
      object 
      cont-ok
      cont-not-supported
      cont-no-alloc
      )
    (declare (ignore state cont-not-supported cont-no-alloc)) ; should do something with cont-no-alloc
    (let*(
           (connection-point (cdr (HA tm)))
           (new-cell (cons object connection-point))
           )
      (rplacd (HA tm) new-cell)
      (funcall cont-ok)
      ))
  ;; Allocates a cell to the right of the head.
  (defmethod a-0
    (
      (tm tm-list)
      (state parked)
      object 
      cont-ok
      cont-not-supported
      cont-no-alloc
      )
    (a◧-0 tm state object cont-ok cont-not-supported cont-no-alloc)
    )
  (defmethod a-0
    (
      (tm tm-list)
      (state void) ; will transition to parked
      object 
      cont-ok
      cont-not-supported
      cont-no-alloc
      )
    (declare (ignore state cont-not-supported cont-no-alloc))
    (setf (tape tm) (cons object ∅))
    (setf (state tm) parked)
    (funcall cont-ok)
    )

;;--------------------------------------------------------------------------------
;; deallocating cells
;;
  ;; deallocates all cells on the tape
  ;; entanglement accounting, transition to void, and spilling is handled by the caller
  ;; void and parked cases handled by the caller
  (defmethod d*-0
    (
      (tm tm-list)
      (state active)
      cont-ok
      cont-not-supported 
      )
    (declare (ignore state cont-not-supported))
    (setf (tape tm) ∅)
    (funcall cont-ok)
    )

  ;; deallocates the leftmost cell
  ;; entanglement accounting, transition to void, and spilling is handled by the caller
  ;; void and parked cases handled by the caller
  (defmethod d◧-0
    (
      (tm tm-list)
      (state active)
      cont-ok
      cont-not-supported
      )
    (declare (ignore state cont-not-supported))
    (setf (tape tm) (cdr (tape tm)))
    (funcall cont-ok)
    )

  ;; deallocates the cell just to the right of the head
  ;; entanglement accounting, transition to void, and spilling is handled by the caller
  ;; void and parked cases handled by the caller
  (defmethod d-0
    (
      (tm tm-list)
      (state active)
      cont-ok
      cont-not-supported 
      )
    (declare (ignore state cont-not-supported))
    (let*(
           (dealloc-cell (cdr (HA tm)))
           (connection-point (cdr dealloc-cell))
           )
      (rplacd (HA tm) connection-point)
      (funcall cont-ok)
      ))

