#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Multi must specialize entangled copy operations so that they add the new or recycled copy
instance to the entanglement list.  The entanglement scope operation must be redefined to
remove the scoped entangled copy from the entanglement list (to distentangle it).

Multi must specialize the deallocation operations to check for a collision and if it finds
one to take the cont-collision continuation instead of performing the deallocation.
Changes to the tape reference must be broadcast to all entangled machines.


|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; cell allocation
;;
  ;; add a new leftmost
  (defgeneric a◧
    (
      tm
      object
      &optional
      cont-ok ;  (be t)
      cont-no-alloc ; (λ()(error 'alloc-fail)))
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
                  cont-ok ; #'echo
                  cont-rightmost ;(λ()(error 'dealloc-on-rightmost))
                  cont-no-alloc ;(λ()(error 'alloc-fail))
                  &rest ⋯
                  )
    (:documentation
      "Deallocate the right neighbor of the cell the head is on.
       I.e. Deallocates a region of length 1 located to the right of the head.
       Returns the object from the deallocated cell.
       If spill is not ∅, the deallocated cell is moved to spill, or a new
       cell is allocated to spill and the object reference is moved there.
      "
      ))

  (defgeneric d◧ (
                   tm 
                   &optional 
                   spill 
                   cont-ok ; #'echo
                   cont-collision ; (λ()(error 'dealloc-collision))
                   cont-no-alloc ; (λ()(error 'alloc-fail))
                   )
    (:documentation
      "Deallocates leftmost.
       Returns the object from the deallocated cell.
       If spill is not ∅, the deallocated cell is moved to spill, or a new
       cell is allocated to spill and the object reference is moved there.
      "
      ))


    


