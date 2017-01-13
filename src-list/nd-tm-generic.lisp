#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These functions derived from non-destructive primitives. 

There is no functional need for a new tape machine implementation to specialize these
functions.  

I wonder if many of these should be made into regular functions so as to improve
performance.

|#
(in-package #:tm)


;;--------------------------------------------------------------------------------
;; copying
;;  
  (def-function-class with-mk-entangled (tm continuation)
    (:documentation
      "Calls continuation with a locally scoped entangled copy of tm.
       "))

  ;; this becomes more interesting when we have entanglement accounting
  (defun-typed with-mk-entangled
    (
      (tm0 nd-tape-machine)
      continuation
      )
    (let(
          (tm1 (mk (type-of tm0) tm0))
          )
      (funcall continuation tm1)
      ))


;;--------------------------------------------------------------------------------
;; leftmost read and write
;;
  (def-function-class r◧ (tm &rest ⋯)
    (:documentation
      "Read leftmost.
      "
      ))

  (defun-typed r◧ ((tm nd-tape-machine) &rest ⋯)
    (declare (ignore ⋯))
    (with-mk-entangled tm
      (λ(tm1)
        (cue-leftmost tm1)
        (r tm1) 
        )))

  (def-function-class w◧ (tm instance &rest ⋯)
    (:documentation
      "Write leftmost.
      "
      ))

  (defun-typed w◧ ((tm nd-tape-machine) instance &rest ⋯)
    (declare (ignore ⋯))
    (with-mk-entangled tm
      (λ(tm1)
        (cue-leftmost tm1)
        (w tm1 instance) 
        )))

;;--------------------------------------------------------------------------------
;; stepping with a boundary, boundaries are inclusive
;;
  ;; although we don't make any copies in this function, we do have two tape
  ;; machines that are on the same tape.  That can not happen with a solo machine
  ;; so nd-tape-machine is as far up the inheritance tree that this can go.
  (def-function-class s≠ 
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

  (defun-typed s≠ 
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
;; cell allocation
;;
  (def-function-class a◨
    (
      tm
      instance
      &optional
      cont-ok
      cont-no-alloc
      )
    (:documentation
      "Allocates a cell to the right of rightmost (thus becoming the new rightmost).
      "
      ))

  (defun-typed a◨
    (
      tm
      instance
      &optional
      (cont-ok (be t))
      (cont-no-alloc #'alloc-fail)
      )
    "Allocates a cell to the right of rightmost (thus becoming the new rightmost)."
    (with-mk-entangled tm
      (λ(tm1)
        (cue-rightmost tm1)
        (a tm1 instance cont-ok cont-no-alloc)
        )))
