#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

We never make  empty machines, rather managed machines are change classed
to this type after a request to delete the last cell from the tape
belonging to a machine that has a parked head.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; status-tm definitions
;;
  ;; an empty machine is already parked
  (defun-typed hp ((tm empty) &optional ➜)
     (declare (ignore tm))
     (destructuring-bind
       (
         &key
         (➜ok (be t))
         &allow-other-keys
         )
       ➜
       [➜ok]
       ))

;;--------------------------------------------------------------------------------
;; tm-decl-only
;;

  (def-empty-1 r)

  (defun-typed esr ((tm empty) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))


  (def-empty-1 w instance)

  (defun-typed esw ((tm empty) instance &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  (def-empty-1 h◧)

  (defun-typed s ((tm empty) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  (defun-typed -s ((tm empty) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (
        &key
        (➜leftmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜leftmost]
      ))

  ;; adding a cell to an empty machine as the same as adding a cell to 
  ;; leftmost from an empty machine
  ;; specialized types may depend on this synonym being present, and thus not
  ;; implement their own #'a
  ;;
    (defun-typed a ((tm empty) instance &optional ➜)
      (a◧ tm instance ➜)
      )

  ;; note the doc on imprecate middle, this might be revised
  ;; on leftmost implies the machine has been rewound is is ready
  (defun-typed on-leftmost ((tm empty) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜t        (be t))
        &allow-other-keys
        )
      ➜
      [➜t]
      ))

  ;; note the doc on imprecate middle, this might be revised
  ;; there are no cells to the right
  ;; can not step right from parked when the machine is empty
  (defun-typed on-rightmost ((tm empty) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜t        (be t))
        &allow-other-keys
        )
      ➜
      [➜t]
      ))

  (defun-typed tape-length-is-one ((tm active) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))
      
  (defun-typed tape-length-is-two ((tm active) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))


;;--------------------------------------------------------------------------------
;; tm-generic
;;
  (def-empty-1 h◨)

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (defun-typed a◧ ((tm empty) instance &optional ➜) 
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      ;; (prins (print "a◧ empty"))
      ;; address rightmost will already be zero
      ;; address will already be zero
      (w (base tm) instance)
      (to-parked tm)
      [➜ok]
      ))

  (defun-typed d ((tm empty) &optional spill ➜)
    (declare (ignore tm spill))
    (destructuring-bind
      (
        &key
        (➜empty #'use-of-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))

  (defun-typed d◧ ((tm empty) &optional spill ➜)
    (declare (ignore tm spill))
    (destructuring-bind
      (
        &key
        (➜empty #'use-of-empty)
        &allow-other-keys
        )
      ➜
      ;; (prins (print "d◧ empty"))
      [➜empty]
      ))

  (defun-typed d. ((tm empty) &optional spill ➜)
    (declare (ignore tm spill))
    (destructuring-bind
      (&key
        (➜fail (λ()(error 'use-of-empty)))
        &allow-other-keys
        )
      ➜
      [➜fail]
      ))

;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;   if a machine in an entanglement group is empty, then all machines
;;   in the entanglement group are empty.  An empty machine is considered to
;;   to have a parked head, so there is a 'heads-on-same-cell' semantic.
;;
  (defun-typed heads-on-same-cell 
    (
      (tm0 empty)
      (tm1 empty)
      &optional ➜
      )
    (entangled tm0 tm1 ➜)
    )
 
  (defun-typed heads-on-same-cell
    (
      (tm0 empty)
      (tm1 status-tm)
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
      (tm0 status-tm)
      (tm1 empty)
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

;;--------------------------------------------------------------------------------
;; nd-tm-generic
;;

  ;; by contract, tm0 and tm1 are entangled
  ;; because they are entangled they are of the same type (thus far in this implementation)
  ;; tm0 empty <=> tm1 empty
  ;; tm0 abandoned <=> tm1 abandoned
  ;; tm0 parked, tm1 can be active, vice versa
  (defun-typed s≠ 
    (
      (tm0 empty)
      (tm1 empty)
      &optional ➜
      )
    (declare (ignore tm0 tm1))
    (destructuring-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  (defun-typed a◨ ((tm empty) instance &optional ➜)
    (a◧ tm instance ➜)
    )
        
