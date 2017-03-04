#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Currently these are supported status:

abandoned
parked
empty
active

There is no function on the tm interface that can be called to change the status
of an active machine.  'delete' of the last cell, for example, will result in 
a collision error.  Hence behavior is inherited from the identity transform.

Should we check to see if two machines are entangled, and bail early if not, for
heads-on-same-cell?

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; status-tm definitions
;;
  (defun-typed park ((tm status-active) &optional ➜)
     (declare (ignore tm))
     (destructuring-bind
       (
         &key
         (➜ok (be t))
         &allow-other-keys
         )
       ➜
       (change-class tm 'status-parked)
       [➜ok]
       ))


;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
  (defun-typed heads-on-same-cell 
    (
      (tm0 status-active)
      (tm1 status-active)
      &optional ➜
      )
    (entangled tm0 tm1
      {
        :➜t (λ()
              (heads-on-same-cell (base tm0) (base tm1) ➜)
              )
        :➜∅ (λ()
              (destructuring-bind
                (
                  &key
                  (➜∅ (be ∅))
                  &allow-other-keys
                  )
                ➜
                [➜∅]
                ))
        }))
 
  (defun-typed heads-on-same-cell
    (
      (tm0 status-active)
      (tm1 tape-machine)
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

  (defun-typed heads-on-same-cell
    (
      (tm0 tape-machine)
      (tm1 status-active)
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))
