#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; making list machines from other instances
;;
  (defun-typed init 
    (
      (tm list-ea-tm)
      (keyed-parms cons)
      &optional ➜
      )
    (setf (entanglements tm) (mk 'list-solo-tm {:tape {tm}})) ; initially entangled only with self
    (call-next-method tm keyed-parms ➜) ; falls to init for tm-list
    )

  (defun-typed init 
    (
      (tm1 list-ea-tm)
      (tm0 list-ea-tm) ; make an entangled copy of tm0
      &optional ➜
      )
    (setf (entanglements tm1) (entanglements tm0))
    (let(
          (ents (entanglements tm1))
          )
      (cue-leftmost ents)
      (∃ ents ;; if tm1 does not exist in the entanglements list, add it
        (λ(ents ➜t ➜∅) (if (eq (r ents) tm1) [➜t] [➜∅]))
        #'do-nothing
        (λ()(a&h◨ ents tm1))
        )
      (call-next-method tm1 tm0 ➜) ; falls to list-nd-tm entangled copy
      ))

