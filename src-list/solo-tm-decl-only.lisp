#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; cell allocation
;;
  ;; add a new leftmost
  (def-function-class a◧
    (
      tm
      instance
      &optional
      cont-ok ;  (be t)
      cont-no-alloc ; #'alloc-fail)
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
;; the instance from the deallocated cell is moved to it, preferably the former. 
;;
;; d must have transactional behavior, i.e. the cell is only dealloced if all goes well,
;; otherwise d makes no structural changes.  E.g. d will fail if spill is not nil, and
;; reallocation to spill fails
;;
  (def-function-class d (
                  tm 
                  &optional 
                  spill 
                  cont-ok ; #'echo
                  cont-rightmost ;(λ()(error 'dealloc-on-rightmost))
                  cont-no-alloc ;#'alloc-fail
                  &rest ⋯
                  )
    (:documentation
      "Deallocate the right neighbor of the cell the head is on.
       I.e. deallocates a region of length 1 located to the right of the head.
       Returns the instance from the deallocated cell.
       If spill is not ∅, the deallocated cell is moved to spill, or a new
       cell is allocated to spill and the instance reference is moved there.
      "
      ))

  (def-function-class d◧ (
                   tm 
                   &optional 
                   spill 
                   cont-ok ; #'echo
                   cont-collision ; (λ()(error 'dealloc-collision))
                   cont-no-alloc ; #'alloc-fail
                   )
    (:documentation
      "Deallocates leftmost.
       Returns the instance from the deallocated cell.
       If spill is not ∅, the deallocated cell is moved to spill, or a new
       cell is allocated to spill and the instance reference is moved there.
      "
      ))



