#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These functions derived from non-destructive primitives. 

There is no functional need for a new tape machine implementation to specialize these
functions.  


|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; copying
;;  
  (defun recycle-entangled-with (tm-to-be-recycled tm-orig)
    "Like mk-entangled-with, but we initialize tm-to-be-recycled rather than creating a new
     instance.  This operation plays a role in stepping across heterogenous types, such as
     occurs in step into for subspaces.
     "
    (recycle-entangled-with-0 tm-to-be-recycled tm-orig (state tm-orig))
    )

  (defun mk-entangled-with (tm-orig)
    "Make a new tape machine. Initializes the new machine by entangling it
     with tm-orig.  An entangled machine shares a tape, but has an independent
     head. Returns the new machine.
     "
    (mk-entangled-with-0 tm-orig (state tm-orig))
    )

  (defun mk-shallow-copy
    (tm-orig
      &optional
      (cont-ok #'echo)
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "Makes a new tape machine.  Initializes the tape with a copy of the
     tape found in tm-orig.  The new tape references the same objects at the
     tm-orig tape.  The new machine is not entangled with the tm-orig machine.
     "
    (let(
          (tm-copy (make-instance (type-of tm-orig)))
          )
      (as* tm-copy tm-orig
        (λ()(funcall cont-ok tm-copy))
        cont-no-alloc
        )))

   ;; recycle-shallow-copy .. hmmm

;;--------------------------------------------------------------------------------
;; leftmost read and write
;;
  (defun r◧
    (
      tm
      &optional
      (cont-ok #'echo)
      (cont-void (λ()(error 'access-void)))
      )
    "read leftmost cell of the tape"
    (r◧-0 tm (state tm) cont-ok cont-void)
    )
  (defgeneric r◧-0 (tm tm-state cont-ok cont-void))
  (defmethod r◧-0  (tm (tm-state void) cont-ok cont-void)
    (declare (ignore tm cont-ok))
    (funcall cont-void)
    )
  (defmethod r◧-0  ((tm nd-tape-machine) (tm-state active) cont-ok cont-void)
    (declare (ignore cont-void))
    (let(
          (tm1 (mk-entangled-with-0 tm))
          )
      (cue-leftmost tm1)
      (r tm1 cont-ok #'cant-happen) ; cue-leftmost would have unparked the head
      ))
  (defmethod r◧-0  ((tm nd-tape-machine) (tm-state parked) cont-ok cont-void)
    (r◧-0 tm active cont-ok cont-void)
    )

  (defun w◧
    (
      tm
      object
      &optional
      (cont-ok #'echo)
      (cont-void (λ()(error 'access-void)))
      )
    "read leftmost cell of the tape"
    (w◧-0 tm (state tm) object cont-ok cont-void)
    )
  (defgeneric w◧-0 (tm tm-state object cont-ok cont-void))
  (defmethod w◧-0  (tm (tm-state void) cont-ok cont-void)
    (declare (ignore tm cont-ok))
    (funcall cont-void)
    )
  (defmethod w◧-0  ((tm nd-tape-machine) (tm-state active) object cont-ok cont-void)
    (declare (ignore cont-void))
    (let(
          (tm1 (mk-entangled-with-0 tm))
          )
      (cue-leftmost tm1)
      (w tm1 object cont-ok #'cant-happen) ; cue-leftmost would have unparked the head
      ))
  (defmethod w◧-0  ((tm nd-tape-machine) (tm-state parked) object cont-ok cont-void)
    (w◧-0 tm active object cont-ok cont-void)
    )

;;--------------------------------------------------------------------------------
;; stepping with a boundary, boundaries are inclusive
;;
  (defun s≠ 
    (
      tm0
      tm1
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      (cont-bound (be ∅))
      )
    "tm0 and tm1 are on the same tape.  Step tm0 unless it is equal to tm1.
     If tm0 reaches rightmost, but it still isn't on the same cell as tm1
     then cont-rightmost.
    "
    (heads-on-same-cell tm0 tm1
      cont-bound
      (λ()(s tm0 cont-ok cont-rightmost))
      ))

;;--------------------------------------------------------------------------------
;; location
;;  
  (defun on-leftmost 
    (
      tm
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    "tm head is on the leftmost cell."
    (on-leftmost-0 tm (state tm) cont-true cont-false)
    )
  (defgeneric on-leftmost-0 (tm state cont-true cont-false))
  (defmethod on-leftmost-0 (tm (state void) cont-true cont-false)
    (declare (ignore tm state cont-true))
    (funcall cont-false)
    )
  (defmethod on-leftmost-0 (tm (state parked) cont-true cont-false)
    (declare (ignore tm state cont-true))
    (funcall cont-false)
    )
  (defmethod on-leftmost-0 (tm (state active) cont-true cont-false)
    (let(
          (tm1 (mk-entangled-with tm))
          )
      (cue-leftmost tm1)
      (heads-on-same-cell tm1 tm cont-true cont-false)
      ))

  (defun on-rightmost
    (tm
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    "tm head is on the rightmost cell."
    (on-rightmost-0 tm cont-true cont-false)
    )
  (defgeneric on-rightmost-0 (tm cont-true cont-false))
  (defmethod on-rightmost-0 (tm cont-true cont-false)
    (let(
          (tm1 (mk-entangled-with tm))
          )
      (s tm1 cont-false cont-true)
      ))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
;; Allocated cells must be initialized.  The initialization value is provided
;; directly or though a fill machine.
;;
  (defun a◨
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "Allocates a cell to the right of rightmost (thus becoming the new rightmost)."
    (a◨-0 tm (state tm) object cont-ok cont-no-alloc)
    )
  (defgeneric a◨-0 (tm state object cont-ok cont-no-alloc))
  (defmethod a◨-0 (tm (state void) object cont-ok cont-no-alloc)
    (declare (ignore state))
    ;; ironic, allocating to rightmost from void is the same as allocating to leftmost
    (a◧ tm object cont-ok cont-no-alloc)
    )
  (defmethod a◨-0 (tm state object cont-ok cont-no-alloc)
    (declare (ignore state))
    (let(
          (tm1 (mk-entangled-with tm))
          )
      (cue-rightmost tm1)
      (a tm1 object cont-ok cont-no-alloc)
      ))
