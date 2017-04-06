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
;; entanglement
;;
  (defun-typed with-entangled ((tm nd-tape-machine) continuation)
    [continuation (entangle tm)]
    )


;;--------------------------------------------------------------------------------
;; printing
;;
  (def-function-class tm-print (tm))
  
  (defun tm-print-1 (tm0 tm1)
    (if 
      (heads-on-same-cell tm0 tm1)
      (progn
        (princ "[")
        (princ (r tm1))
        (princ "]")
        )
      (princ (r tm1))
      ))

  (defun-typed tm-print ((tm0 nd-tape-machine))
    (with-entangled tm0
      (λ(tm1)
        (c◧ tm1)
        (tm-print-1 tm0 tm1)
        (s tm1 
          {
            :➜ok (λ()
                   (∀* tm1 (λ(tm1)(princ " ")(tm-print-1 tm0 tm1)))
                   )
            })
        t
        )))
        
;;--------------------------------------------------------------------------------
;; stepping with a boundary, boundaries are inclusive
;;
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
      (with-entangled tm
        (λ(tm1)
          (c◨ tm1)
          (a tm1 instance {:➜ok ➜ok :➜no-alloc ➜no-alloc})
          ))))

