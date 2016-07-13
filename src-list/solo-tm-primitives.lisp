#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


Destructive operations are allowed on solo machines.

Even with these destructive operations, solo machines can not be recycle-entangled-with or mk-entangled-with.
This is because copy operations would cause the tape to become shared.  Without copying,
we can not make temporary variables that have independent head movement from the machine
they were copied from.  This prevents us from implementing some derived methods that 
exist for nd-tape-machines.


|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; cell allocation
;;
  ;; add a new leftmost cell to the specified machine
  (defgeneric a◧
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (:documentation
      "Allocates a cell to the left of leftmost (thus becoming the new leftmost).
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
;; otherwise d makes no structural changes.  E.g. d will fail if spill is not nil, and
;; reallocation to spill fails
;;
  (defgeneric d (
                  tm 
                  &optional 
                  spill 
                  (cont-ok #'echo)
                  (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
                  (cont-no-alloc (λ()(error 'alloc-fail)))
                  &rest ⋯
                  )
    (:documentation
      "Deallocate the cell just to the right of the head. (Deallocates a region of length 1.)
      "
      ))

  (defgeneric d (tm &optional cont-ok cont-rightmost cont-no-alloc))

  (defgeneric d◧ (
                   tm 
                   &optional 
                   spill 
                   (cont-ok #'echo)
                   (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
                   (cont-no-alloc (λ()(error 'alloc-fail)))
                   (cont-collision (λ()(error 'dealloc-entangled)))
                   )
    (:documentation
      "The leftmost cell is deallocated independent of where the head is located.
      "
      ))


    


