#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These functions derived from non-destructive primitives. 

implementations inherit from nd-tape-machine, so they fall back to here to find the 
generic implementations.

There is no functional need for a new tape machine implementation to specialize these
functions.  

|#
(in-package #:tm)


;;--------------------------------------------------------------------------------
;; stepping with a boundary, boundaries are inclusive
;;
  ;; although we don't make any copies in this function, we do have two tape
  ;; machines that are on the same tape.  That can not happen with a solo machine
  ;; so nd-tape-machine is as far up the inheritance tree that this can go.
  (def-function-class s≠ (tm0 tm1 &optional ➜)
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
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (be ∅))
        (➜bound (be ∅))
        &allow-other-keys
        )
      ➜
      (heads-on-same-cell tm0 tm1
        {
          :➜t ➜bound
          :➜∅ (λ()(s tm0 {:➜ok ➜ok :➜rightmost ➜rightmost}))
          })))


;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (def-function-class a◨ (tm instance &optional ➜)
    (:documentation
      "Allocates a cell to the right of rightmost (thus becoming the new rightmost).
      "
      ))

  (defun-typed a◨
    (
      (tm nd-tape-machine)
      instance
      &optional ➜
      )
    "Allocates a cell to the right of rightmost (thus becoming the new rightmost)."
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (let(
            (tm1 (entangle tm))
            )
        (cue-rightmost tm1)
        (a tm1 instance {:➜ok ➜ok :➜no-alloc ➜no-alloc})
        )))

