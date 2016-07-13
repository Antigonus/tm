#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These functions derived from non-destructive primitives. 

There is no functional need for a new tape machine implementation to specialize these
functions.  


|#
(in-package #:tm)


;;--------------------------------------------------------------------------------
;; copying
;;  
  ;; this becomes more interesting when we have entanglement accounting
  (defgeneric with-mk-entangled (tm continuation)
    (:documentation
      "Calls continuation with a locally scoped entangled copy of tm.
       "))

  (defmethod with-mk-entangled
    (
      (tm nd-tape-machine)
      continuation
      )
    (let(
          (tm1 (mk-entangled tm))
          )
      (funcall continuation tm1)
      ))


;;--------------------------------------------------------------------------------
;; leftmost read and write
;;
  (defgeneric r◧ (tm &rest ⋯)
    (:documentation
      "read leftmost cell of the tape
      "
      ))

  (defmethod r◧ ((tm nd-tape-machine) &rest ⋯)
    (declare (ignore ⋯))
    (with-mk-entangled tm
      (λ(tm1)
        (cue-leftmost tm1)
        (r tm1) 
        )))

  (defgeneric w◧ (tm object &rest ⋯)
    (:documentation
      "writes leftmost cell of the tape
      "
      ))

  (defmethod w◧ ((tm nd-tape-machine) object &rest ⋯)
    (declare (ignore ⋯))
    (with-mk-entangled tm
      (λ(tm1)
        (cue-leftmost tm1)
        (w tm1 object) 
        )))

;;--------------------------------------------------------------------------------
;; stepping with a boundary, boundaries are inclusive
;;
  ;; although we don't make any copies in this function, we do have two tape
  ;; machines that are on the same tape.  That can not happen with a solo machine
  ;; so nd-tape-machine is as far up the inheritance tree that this can go.
  (defgeneric s≠ 
    (
      tm0
      tm1
      &optional
      cont-ok
      cont-bound
      cont-rightmost
      )
    (:documentation
      "tm0 and tm1 are on the same tape. 
       If tm0's head is on the same call as tm1's head, take cont-bound.  Otherwise
       if tm0's head is on the rightmost cell, take cont-rightmost.  Otherwise,
       step tm0.
      "
      ))

  (defmethod s≠ 
    (
      (tm0 nd-tape-machine)
      (tm1 nd-tape-machine)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      (cont-bound (be ∅))
      )
    (heads-on-same-cell tm0 tm1
      cont-bound
      (λ()(s tm0 cont-ok cont-rightmost))
      ))

;;--------------------------------------------------------------------------------
;; location
;;  
  (defgeneric on-leftmost 
    (
      tm
      &optional
      cont-true
      cont-false
      )
    (:documentation
      "tm head is on the leftmost cell.
      "))

  (defmethod on-leftmost 
    (
      tm
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (with-mk-entangled tm
      (λ(tm1)
        (cue-leftmost tm1)
        (heads-on-same-cell tm1 tm cont-true cont-false)
        )))

  (defgeneric on-rightmost
    (
      tm
      &optional
      cont-true
      cont-false
      )
    (:documentation
      "tm head is on the rightmost cell.
      "
      ))

  (defmethod on-rightmost
    (
      tm
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (with-mk-entangled tm
      (λ(tm1)
        (s tm1 cont-false cont-true)
        )))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
;; Allocated cells must be initialized.  The initialization value is provided
;; directly or through a fill machine.
;;
  (defgeneric a◨
    (
      tm
      object
      &optional
      cont-ok
      cont-no-alloc
      )
    (:documentation
      "Allocates a cell to the right of rightmost (thus becoming the new rightmost).
      "
      ))

  (defmethod a◨
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "Allocates a cell to the right of rightmost (thus becoming the new rightmost)."
    (with-mk-entangled tm
      (λ(tm1)
        (cue-rightmost tm1)
        (a tm1 object cont-ok cont-no-alloc)
        )))
