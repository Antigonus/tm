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
;;   
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

;;--------------------------------------------------------------------------------
;; cell deallocation
;;
;; Spill can be ∅, in which case we just drop the deallocated cell.  When spill is not ∅,
;; then the deallocated cell is moved to spill, or a new allocation is made on spill and
;; the object from the deallocated cell is moved to it, preferably the former. 
;;
;; d must have transactional behavior, so if the cont-no-alloc exit is to be taken,
;; then tm must remain uneffected.  I.e. the cell can't just be dropped, as then
;; continuation might not be possible.
;;
;; There are two reasons deallocation might fail a) because there is nothing
;; to deallocate,  b) because the tape does not support structural changes.  As
;; a third problem, the reallocation to spill might fail.
;;
;; need to rename cont-rightmost as cont-no-dealloc in other parts of the code
;;
;; both deallocation and allocation are really the same thing, moving cells from
;; one space to another
;;
  (defgeneric d (tm &optional spill cont-ok cont-no-dealloc cont-no-alloc)
    (:documentation 
      "Deallocates one cell to the right of the head. If there is no such cell,
       #'d takes cont-no-dealloc. Otherise if spill does not exist, the cell is dropped,
       and #d takes cont-ok, passing it the object from the deallocated cell.  If spill
       does exist then either the deallocated cell is moved to spill, or the object from
       the deallocated cell is moved to a new allocation on spill. In the latter
       case, if the allocation fails #'d takes cont-no-alloc.  Otherwise 
       cont-ok is called with the object from the deallocated cell.
       "
      ))

