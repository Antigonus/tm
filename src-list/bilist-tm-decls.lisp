#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; head stepping
;;
  (def-function-class -s (tm &optional ➜)
    (:documentation
      "If the head is on a cell, and there is a left neighbor, puts the head on the
       left neighbor and ➜ok.  If there is no left neighbor, then ➜leftmost.
       "))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (def-function-class -a (tm instance &optional ➜)
    (:documentation
    "If no cells are available, ➜no-alloc.  Otherwise, allocate a new cell and place
     it to the left of the cell the head is currently on.  The newly allocated cell will
     be initialized with the given instance.
     "))

;;--------------------------------------------------------------------------------
;; cell deallocation
;;
;; Spill can be ∅, in which case we just drop the deallocated cell.  When spill is not ∅,
;; then the deallocated cell is moved to spill, or a new allocation is made on spill and
;; the instance from the deallocated cell is moved to it, preferably the former. 
;;
;; d must have transactional behavior, i.e. the cell is only dealloced if all goes well,
;; otherwise d makes no structural changes.  E.g. d will fail if spill is not nil, and
;; reallocation to spill fails
;;
  (def-function-class -d (tm &optional spill ➜)
    (:documentation
      "Deallocate the left neighbor of the cell the head is on.
      "
      ))



