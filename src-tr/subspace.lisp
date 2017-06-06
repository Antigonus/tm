#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Tree is just a tape machine that supports methods for traversing a tree. 

Instances of type 'subspace-tr are understood to be nodes in the tree.  The
base type of a 'subspace-tr is a tape machine. This is taken to be a first 
level tape machine, one that does not support status, or at least will never
be empty.

A machine marked as a subspace brings with it new functionality to
tape machines:

     step-depth
     step-breadth
     step-down - pushes return on stack
     step-up  - uses return
     step-down primitive - steps into subspace with no ability to call step-up later
     on-subspace - (typep instance 'subspace)

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a tape machine
;;
  (def-type subspace-tr (idenity-tr)()) ;; base type is not a status type
  (def-type tree-tr (identity-tr)
    (
      (history ; a status tm for holding the history
        :initarg history
        :accessor history
      )))

;;--------------------------------------------------------------------------------
;;
  ;; (defun-typed a-subspace  epa-subspace

  (defun-typed s-into (i (tm tree-tr))
    (declare (ignore i tm))
    )
  (defun-typed s-into ((i subspace-tr) (tm tree-tr))
    (epa (history tm) (base tm))
    (setf (base tm) (base i))
    (-s* tm)
    (s-into (r tm) tm)
    )

  (defun-typed step-depth ((tm tree-tr) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (s (base tm)
        {
          :➜ok 
          (λ()
            (s-into (r tm) tm)
            [➜ok]
            )

          :➜rightmost
          (λ()
            (r (history tm)
              {
                :➜empty ➜rightmost
                :➜ok
                (λ()
                  (epd (history tm)
                    {
                      :➜ok
                      (λ(instance)
                        (setf (base tm) instance)
                        (s tm ➜ok ➜rightmost)
                        )
                      :➜rightmost #'cant-happen
                      :➜no-alloc #'cant-happen ; no spill
                      }))
                }))
          })))

  (defun-typed step-breadth ((tm tree-tr) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (s (base tm)
        {
          :➜ok 
          (λ()
            (s-into (r tm) tm)
            [➜ok]
            )
          :➜rightmost
          (λ()
            )
          })
      ))
