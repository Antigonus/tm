#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; cell deallocation
;;
;; Spill can be ∅, in which case we just drop the deallocated cell.  When spill is not ∅,
;; then the deallocated cell is moved to spill, or a new allocation is made on spill and
;; the instance from the deallocated cell is moved to it, preferably the former. 
;;
;; d. must have transactional behavior, i.e. the cell is only dealloced if all goes well,
;; otherwise d makes no structural changes.  E.g. d. will fail if spill is not nil, and
;; reallocation to spill fails
;;
  (def-function-class d. (tm &optional spill ➜)
    (:documentation
      "Deallocate the indicated cell. Orphans tm while doing so, which is
       why this is a 'haz' operation. The status machine will abandon
       tm, but the first level machines have no way of doing so.
      "
      ))


