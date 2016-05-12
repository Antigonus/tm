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
  (defgeneric r (tm)
    (:documentation 
      "Given a tape machine, returns the object from the cell under the tape head.")
    )

  (defgeneric w (tm object)
    (:documentation "Writes object into the cell under the tape head.")
    )

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defgeneric cue-leftmost (tm)
    (:documentation 
      "Cue tm's head to the leftmost cell.
       "
      ))

;;--------------------------------------------------------------------------------
;; head location predicate
;;
  (defgeneric heads-on-same-cell (tm0 tm1 &optional cont-true cont-false)
    (:documentation "tm0 and tm1 heads are on the same cell")
    )

;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defgeneric s (tm &optional cont-ok cont-rightmost)
    (:documentation 
      "If the head is on a cell, and there is a right neighbor, puts the head on the
       right neighbor and cont-ok.  If there is no right neighbor, then cont-rightmost.
      "))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
;; Entanglement accounting complicates the swap trick for implementing a◧, so I have made
;; it a primitive.
;;
  (defgeneric a (tm object &optional cont-ok cont-no-alloc)
    (:documentation
      "If no cells are available, cont-no-alloc.  Otherwise, allocate a new cell and place
       it to the right of the cell the head is currently on.  The newly allocated cell
       initialized with the given object.  There are two reasons allocation might fail, a)
       because memory has been exhausted, b) because the tape does not support structural
       changes. (The current implementation throws a system error when the heap is
       depleted.)
       "
      ))
  
  ;; a◧ implementations must work even when the tape is initially ∅, as this will be
  ;; the case when a machine is expanded from tm-void.
  (defgeneric a◧ (tm object &optional cont-ok cont-no-alloc)
    (:documentation
      "Allocates a new leftmost cell. The old leftmost becomes the new cell's right
      neighbor.
      "
      ))

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
  (defgeneric d (tm &optional spill 
                  cont-ok 
                  cont-rightmost
                  cont-not-supported
                  cont-collision
                  cont-no-alloc)
    (:documentation 
      "Deallocates one cell to the right of the head. If there is no such cell,
       #'d takes cont-rightmost.  If the machine does not allow deallocation, then #'d
      takes cont-not-supported.  If another machine is entangled and has its head on the
      deallocation cell, then #'d takes cont-collision.  If spill is not ∅ and realloction
      to spill fails, then the #'d takes cont-no-alloc.  Otherwise #'d takes cont-ok.
      Structural changes are only made when #'d takes cont-ok.
       "
      ))

  (defgeneric d◧ (tm &optional spill 
                   cont-ok
                   cont-rightmost
                   cont-not-supported
                   cont-collision
                   cont-no-alloc
                   )
    (:documentation 
      "Similar to #'d but the leftmost cell is deallocated independent of where the head
       is located. If the tape is parked and singleton, calling d◧ will cause the machine
       to collapse to void.
       "
      ))

;;--------------------------------------------------------------------------------
;; state transition
;;

  ;; voids this machine, does not look at entanglements set
  (defgeneric void (tm)
    (:documentation
      "When deallocation causes the machine to collapse to void, this function
       is called."
      ))

  ;; This works if the tm does not need parameters.
  (defmethod void ((tm tape-machine))
    (change-class tm 'tm-void)
    (setf (HA tm) (type-of tm))
    (setf (tape tm) ∅)
    ;; entanglements are preserved
    )

;;--------------------------------------------------------------------------------
;; copying
;;
  (defgeneric cue-to-1
    (
      tm-cued ; object affected, contents get clobbered
      tm-orig ; remains undisturbed
      )
    (:documentation "Used internally.  Common code between cue-to and dup.")
    )

  (defmethod cue-to-1
    (
      tm-cued 
      (tm-orig tape-machine)
      )
    (setf (HA tm-cued) (HA tm-orig))
    (setf (tape tm-cued) (tape tm-orig))
    (setf (parameters tm-cued) (parameters tm-orig))
    (setf (entanglements tm-cued) (entanglements tm-orig))
    (a (entanglements tm-cued) tm-cued #'do-nothing #'cant-happen)
    tm-cued
    )

