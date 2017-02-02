#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  non-destructive operation primitives

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; entanglement
;;
  (defun-typed entangled
    (
      (tm0 list-nd-tm)
      (tm1 list-nd-tm)
      )
    (destructuring-bind
      (&key
        (:➜t (be t))
        (:➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if (eq (tape tm0) (tape tm1) [➜t] [➜∅]))
    )

;;--------------------------------------------------------------------------------
;; head location
;;
;;

  ;; Though entangled copy function is not directly called in heads-on-same-cell, a copy
  ;; is implied because the function accepts two state machines that share a tape, tm0 and
  ;; tm1.
  (defun-typed heads-on-same-cell 
    (
      (tm0 list-nd-tm)
      (tm1 list-nd-tm)
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if (eq (head tm0) (head tm1)) [➜t] [➜∅])
      ))

  ;; currently when two machines are entangled, they have the same type, ergo if the
  ;; types are different the machines are different, and heads can not be on the same cell
  (defun-typed heads-on-same-cell
    (
      (tm0 list-nd-tm)
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
      (tm1 list-nd-tm)
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

  
