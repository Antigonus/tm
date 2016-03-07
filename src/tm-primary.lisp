#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; the base type
;;
  (defclass tape-machine ()
    (
      (HA 
        :initarg :HA 
        :accessor HA
        )
      (tape
        :initarg :tape
        :accessor tape
        )
      ))

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
       This method might not be available for all implementations."
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

  ;; most machines will consider #'s primary rather than #'so
  (defgeneric so (tm &optional cont-ok cont-rightmost)
    (:documentation 
      "Like #'s, but has a different specialization.
       #'s will specialize into 'step according to traversal path', while #'so
       specializes into 'step over'.  They are identical for lists.
       "))

  (defgeneric s (tm &optional cont-ok cont-rightmost)
    (:documentation 
      "Step tm head to the neighbor cell on the right.
      "))

  (defmethod s (tm &optional cont-ok cont-rightmost)
    (so tm cont-ok cont-rightmost)
    )


;;--------------------------------------------------------------------------------
;; cell allocation
;;

  ;; Relative.
  (defgeneric a (tm object &optional cont-ok cont-no-alloc)
    (:documentation
      "If no cells are available to be allocated then #'a takes the cont-no-alloc
       continuation.  Otherwise, it allocates a new cell and places it to the right of
       the cell the head is currently on.  The newly allocated cell is initialized with
       the given object.  Allocation failures are quite possible for fixed length
       implementations such as arrays.  The current implementation throws a system
       error if the problem is that the system ran out of memory.
       "
      ))

;;--------------------------------------------------------------------------------
;; cell deallocation
;;
;; Spill can be ∅, in which case we just drop the deallocated cell.  When spill is not ∅,
;; then the deallocated cell is moved to spill.  In this situation we can not have a
;; allocation problem, because tm is providing the allocation.  We may use the function
;; #'g to carry the cell over.  However, sometimes cells can not be moved between
;; different machines.  For example, if spill is an fixed length array, and tm is a linked
;; list.  The cons cell won't help the array much, and in any case, we can't create new
;; array slots.
;;
;; Hence, if spill exists, then d attempts to move the allocation cell, with its object,
;; to spill, but if it can not do this, it moves the object to a new allocation on spill.
;; If we get to this point and it is not possible to create a new allocation, then we
;; take the cont-no-alloc exit.
;;
;; d must have transactional behavior, so if the cont-no-alloc exit is to be taken,
;; then tm must remain uneffected.  I.e. the cell can't just be dropped, as then
;; continuation might not be possible.
;;

  (defgeneric d (tm &optional spill cont-ok cont-rightmost cont-no-alloc)
    (:documentation 
      "Deallocates one cell to the right of the head.
       If spill exists, #'d tries to put the deallocated cell on spill.
       If spill can not take such cells, then it calls (as spill (r tm)),
       to move the object of the deallocated cell. Cont-ok is called with 
       the object from the deallocated cell.
       "
      ))

