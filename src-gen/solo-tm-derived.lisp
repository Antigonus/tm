#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These generic functions are defined only in terms of the solo-tm-primitives.


|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; cell allocation
;;
;; Allocated cells must be initialized.  The initialization value is provided
;; directly or though a fill machine.
;;

  ;; allocate new leftmost cell
  (defun a◧
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "Allocates a cell to the left of leftmost (thus becoming the new leftmost)."
    (a◧-0 tm (state tm) object cont-ok cont-no-alloc)
    )

;;--------------------------------------------------------------------------------
;; cell deallocation
;;
  (defun d (
             tm 
             &optional 
             spill 
             (cont-ok #'echo)
             (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
             (cont-collision (λ()(error 'dealloc-entangled)))
             (cont-no-alloc (λ()(error 'alloc-fail)))
             )
    "Deallocate the cell just to the right of the head. (A region of length 1.)"
    (d-0 tm (state tm) spill cont-ok cont-rightmost cont-collision cont-no-alloc)
    )

  (defun d◧ (
              tm 
              &optional 
              spill 
              (cont-ok #'echo)
              (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
              (cont-collision (λ()(error 'dealloc-entangled)))
              (cont-no-alloc (λ()(error 'alloc-fail)))
              )
    "The leftmost cell is deallocated independent of where the head is located."
    (d◧-0 tm (state tm) spill cont-ok cont-rightmost cont-collision cont-no-alloc)
    )

  (defgeneric d-0 (tm tm-state spill cont-ok cont-rightmost cont-collision cont-no-alloc))

  (defmethod d-0 (tm (tm-state void) spill cont-ok cont-rightmost cont-collision cont-no-alloc)
    (declare (ignore tm tm-state spill cont-ok cont-collision cont-no-alloc))
    (funcall cont-rightmost) ; cont-rightmost follows from a progression of deleting cells from a parked state machine
    )

  (defmethod d-0 (tm (tm-state parked) spill cont-ok cont-rightmost cont-collision cont-no-alloc)
    (d◧-0 tm tm-state spill cont-ok cont-rightmost cont-collision cont-no-alloc)
    )

  (defmethod d-0 (tm (tm-state abandoned) spill cont-ok cont-rightmost cont-collision cont-no-alloc)
    (declare (ignore tm spill cont-ok cont-rightmost cont-collision cont-no-alloc))
    (error 'operation-on-abandoned)
    )


