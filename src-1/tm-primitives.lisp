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

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defun cue-leftmost 
    (
      tm
      &optional
      (cont-ok (be t))
      (cont-parked (λ()(error 'parked-head-use)))
      )
    "Cue tm's head to the leftmost cell."
    (cue-leftmost-0 tm (state tm) cont-ok cont-parked)
    )
  (defgeneric cue-leftmost-0 (tm state cont-ok cont-parked))

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
      (cont-parked  (be ∅))
      )
    "tm0 and tm1 heads are on the same cell"
    (heads-on-same-cell-0 tm0 (state tm0) tm1 (state tm1) cont-true cont-false cont-parked)
    )
  (defgeneric heads-on-same-cell-0 (tm-0 state-tm0 tm1 state-tm1 cont-true cont-false))

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

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defun a◧
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "Allocates a new leftmost cell on this machine. The old leftmost becomes the new
      cell's right neighbor.
      "
    (a◧-0 tm (state tm) object cont-ok cont-no-alloc)
    )
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


  ;; 
  (defun d◧-1
    (
      tm
      &optional 
      (cont-ok (be t))
      (cont-not-supported (λ()(error 'not-supported)))
      )
    "Internal, deallocates the leftmost cell only on this machine. 
    "
    (d◧-0 tm (state tm) cont-ok cont-not-supported)
    )
  (defgeneric d◧-0 (tm state cont-ok cont-not-supported))

  (defun d-1
    (
      tm
      &optional 
      (cont-ok (be t))
      (cont-not-supported (λ()(error 'not-supported)))
      )
    "Internal, deallocates the cell to the left of the head.
    "
    (d-0 tm (state tm) cont-ok cont-not-supported)
    )
  (defgeneric d-0 (tm state cont-ok cont-not-supported))


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

