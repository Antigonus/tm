#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

We do not rewind the machines before running the equality test.  So
this can be used for checking that suffixes are equal.

We say two tape machines are equal when they have equal instances in
corresponding locations of the tape.

Set equal drops the requirement for corresponding locations.


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; abandoned
;;
  (defun-typed tm= ((a abandoned) b &optional ➜)
    (declare (ignore a b ➜))
    (operation-on-abandoned)
    )
  (defun-typed tm= (a (b abandoned) &optional ➜)
    (declare (ignore a b ➜))
    (operation-on-abandoned)
    )

;;--------------------------------------------------------------------------------
;; empty
;;
  (defun-typed tm= ((a empty) (b empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      [➜t]
      ))

  (defun-typed tm= ((a empty) (b tape-machine) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

  (defun-typed tm= ((a tape-machine) (b empty) &optional ➜)
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
  (defun-typed tm= ((a parked) (b parked) &optional ➜)
    (s a
      {
        :➜ok (λ()(s b
                   {
                     :➜ok (λ()(tm= a b ➜))
                     :➜rightmost #'cant-happen
                     }))
        :➜rightmost #'cant-happen
        }))

  (defun-typed tm= ((a parked) (b tape-machine) &optional ➜)
    (s a
      {
        :➜ok (λ()(tm= a b ➜)) ; a will now be active
        :➜rightmost #'cant-happen
        }))

  (defun-typed tm= ((a tape-machine) (b parked) &optional ➜)
    (s b
      {
        :➜ok (λ()(tm= a b ➜)) ; b will now be active
        :➜rightmost #'cant-happen
        }))

      
