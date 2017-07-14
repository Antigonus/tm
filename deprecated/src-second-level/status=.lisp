#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

We do not rewind the machines before running the equivalence test.  So
this can be used for checking that suffixes are equivalent.

We say two tape machines are equivalent when they have equivalent instances in
corresponding locations of the tape.

Would be nice to also have an equivalence test that did not require location 
correspondence so tm's could be used for equivalence between sets.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; abandoned
;;
  (defun-typed equiv ((a abandoned) b &optional ➜)
    (declare (ignore a b ➜))
    (operation-on-abandoned)
    )
  (defun-typed equiv (a (b abandoned) &optional ➜)
    (declare (ignore a b ➜))
    (operation-on-abandoned)
    )

;;--------------------------------------------------------------------------------
;; empty
;;
  (defun-typed equiv ((a empty) (b empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      [➜t]
      ))

  (defun-typed equiv ((a empty) (b tape-machine) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

  (defun-typed equiv ((a tape-machine) (b empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

;;--------------------------------------------------------------------------------
;; parked
;;
  (defun-typed equiv ((a parked) (b parked) &optional ➜)
    (s a
      {
        :➜ok (λ()(s b
                   {
                     :➜ok (λ()(equiv a b ➜))
                     :➜rightmost #'cant-happen
                     }))
        :➜rightmost #'cant-happen
        }))

  (defun-typed equiv ((a parked) (b tape-machine) &optional ➜)
    (s a
      {
        :➜ok (λ()(equiv a b ➜)) ; a will now be active
        :➜rightmost #'cant-happen
        }))

  (defun-typed equiv ((a tape-machine) (b parked) &optional ➜)
    (s b
      {
        :➜ok (λ()(equiv a b ➜)) ; b will now be active
        :➜rightmost #'cant-happen
        }))

      
