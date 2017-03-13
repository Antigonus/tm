#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

   c◧ needs to set the address to zero, stepping increment it etc.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; status-tm specific
;;
  (def-function-class park (tm &optional ➜))

  (defun abandon (tm)
    (change-class tm 'status-abandoned)
    )


;;--------------------------------------------------------------------------------
;; tm-decl-only
;;
  ;; specialized versions have been defined for status-empty and status-abandoned
  (defun-typed tape-length-is-one ((tm status-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if (= (address-rightmost tm) 0) [➜t] [➜∅])
      ))
      
  ;; specialized versions have been defined for status-empty and status-abandoned
  (defun-typed tape-length-is-two ((tm status-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if (= (address-rightmost tm) 1) [➜t] [➜∅])
      ))

;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
  (defun-typed entangled
    (
      (tm0 status-tm)
      (tm1 status-tm)
      &optional ➜
      )
    (entangled (base tm0) (base tm1) ➜)
    )


