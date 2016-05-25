#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

All tape machine implmentations must specialize these functions.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defun r
    (
      tm
      &optional
      (cont-ok #'echo)
      (cont-parked (λ()(error 'parked-head-use)))
      )
    "Given a tape machine, returns the object from the cell under the tape head."
    (r-0 tm (state tm) cont-ok cont-parked)
    )
  (defgeneric r-0 (tm state cont-ok cont-parked))
  (defmethod r-0 (tm (state void) cont-ok cont-parked)
    (declare (ignore tm state cont-ok))
    (funcall cont-parked)
    )

  (defun w 
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-parked (λ()(error 'parked-head-use)))
      )
    "Writes object into the cell under the tape head."
    (w-0 tm (state tm) object cont-ok cont-parked)
    )
  (defgeneric w-0 (tm state object cont-ok cont-parked))
  (defmethod w-0 (tm (state void) object cont-ok cont-parked)
    (declare (ignore tm state object cont-ok))
    (funcall cont-parked)
    )

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defun cue-leftmost
    (
      tm
      &optional
      (cont-ok (be t))
      (cont-void (λ()(error 'access-void)))
      )
    "Cue tm's head to the leftmost cell."
    (cue-leftmost-0 tm (state tm) cont-ok cont-void)
    )
  (defgeneric cue-leftmost-0 (tm state cont-ok cont-void))
  (defmethod cue-leftmost-0 (tm (state void) cont-ok cont-void)
    (declare (ignore tm state cont-ok))
    (funcall cont-void)
    )


;;--------------------------------------------------------------------------------
;; head location predicate
;;
  (defun heads-on-same-cell 
    (
      tm0 
      tm1
      &optional 
      (cont-true (be t))
      (cont-false (be ∅))
      (cont-parked (λ()(error 'parked-head-use)))
      )
    "tm0 and tm1 heads are on the same cell"
    (heads-on-same-cell-0 tm0 (state tm0) tm1 (state tm1) cont-true cont-false cont-parked)
    )
  (defgeneric heads-on-same-cell-0 (tm0 state0 tm1 state1 cont-true cont-false cont-parked))
  (defmethod heads-on-same-cell-0 (tm0 (state0 void) tm1 state1 cont-true cont-false cont-parked)
    (declare (ignore tm0 state0 state1 cont-true cont-false))
    (funcall cont-parked)
    )
  (defmethod heads-on-same-cell-0 (tm0 (state0 parked) tm1 state1 cont-true cont-false cont-parked)
    (declare (ignore tm0 state0 state1 cont-true cont-false))
    (funcall cont-parked)
    )
  (defmethod heads-on-same-cell-0 (tm0 state0 tm1 (state1 parked) cont-true cont-false cont-parked)
    (declare (ignore tm0 state0 state1 cont-true cont-false))
    (funcall cont-parked)
    )

;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defun s
    (
      tm
      &optional 
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    "If the head is on a cell, and there is a right neighbor, puts the head on the
       right neighbor and cont-ok.  If there is no right neighbor, then cont-rightmost.
      "
    (s-0 tm (state tm) cont-ok cont-rightmost)
    )
  (defgeneric s-0 (tm state cont-ok cont-rightmost))
  (defmethod s-0 (tm (state void) cont-ok cont-rightmost)
    (declare (ignore tm state cont-ok))
    (funcall cont-rightmost)
    )

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  ;; see tm-derived-1  for defun a◧
  ;; the job of this primitive is to add a new leftmost cell to the specified machine
  (defgeneric a◧-0 (tm state object cont-ok cont-no-alloc))

  (defun a
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "If no cells are available, cont-no-alloc.  Otherwise, allocate a new cell and place
       it to the right of the cell the head is currently on.  The newly allocated cell
       initialized with the given object.  There are two reasons allocation might fail, a)
       because memory has been exhausted, b) because the tape does not support structural
       changes. (The current implementation throws a system error when the heap is
       depleted.)
       "
    (a-0 tm (state tm) object cont-ok cont-no-alloc)
    )
  (defgeneric a-0 (tm state object cont-ok cont-no-alloc));



;;--------------------------------------------------------------------------------
;; cell deallocation
;;
;; Spill can be ∅, in which case we just drop the deallocated cell.  When spill is not ∅,
;; then the deallocated cell is moved to spill, or a new allocation is made on spill and
;; the object from the deallocated cell is moved to it, preferably the former. 
;;
;; d must have transactional behavior, i.e. the cell is only dealloced if all goes well,
;; otherwise d makes no structural changes. 
;;
;; There are multiple reasons deallocation might fail a) because there is nothing
;; to deallocate,  b) because the tape does not support structural changes c) because
;; another machine has a head on the dealloc cell.
;;
;; d will also fail if spill is not nil, and reallocation to spill fails
;;
;; Entanglement accounting complicates the swap trick for implementing d◧, so I have made
;; it a primitive.
;;
  (defun d*-1
    (
      tm
      &optional 
      (cont-ok (be t))
      (cont-not-supported (λ()(error 'not-supported)))
      )
    "Deallocates the tape.
    "
    (d*-0 tm (state tm) cont-ok cont-not-supported)
    )
  (defgeneric d*-0 (tm state cont-ok cont-not-supported))
  ;; deallocation of a whole tape returns no objects, so we can follow cont-ok
  (defmethod d*-0 (tm (state void) cont-ok cont-not-supported)
    (declare (ignore cont-not-supported))
    (funcall cont-ok)
    )

  (defgeneric d◧-0 (tm state cont-ok cont-not-supported))
  ;; deallocation of a single cell returns the object that was in the cell,
  ;; but we can't do that in a void space, so we follow cont-not-supported
  (defmethod d◧-0 (tm (state void) cont-ok cont-not-supported)
    (declare (ignore cont-ok))
    (funcall cont-not-supported)
    )

  (defgeneric d-0 (tm state cont-ok cont-not-supported))
  (defmethod d-0 (tm (state void) cont-ok cont-not-supported)
    (declare (ignore cont-ok))
    (funcall cont-not-supported)
    )


;;--------------------------------------------------------------------------------
;; copying
;;
;; The base copying requies no entanglement accounting, because it is derived.
;; This is for internal use.
;;
  (defgeneric cue-to-0
    (
      tm-cued ; object affected, contents get clobbered
      tm-orig ; remains undisturbed
      )
    (:documentation "Used internally to make copies sans entanglement accounting.")
    )

  ;; this will work for many machine types
  (defmethod cue-to-0
    (
      tm-cued 
      (tm-orig tape-machine)
      )
    (setf (state tm-cued) (state tm-orig))
    (setf (HA tm-cued) (HA tm-orig))
    (setf (tape tm-cued) (tape tm-orig))
    (setf (parameters tm-cued) (parameters tm-orig))
    tm-cued
    )

