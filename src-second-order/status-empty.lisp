#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

We never make  empty machines, rather managed machines are change classed
to this type after a request to delete the last cell from the tape
belonging to a machine that has a parked head.

|#

(in-package #:tm)

(defmacro def-empty-1 (f &rest args)
  `(defun-typed ,f ((tm status-empty) ,@args &optional ➜)
     (declare (ignore ,@args))
     (destructuring-bind
       (
         &key
         (➜empty #'use-of-empty)
         &allow-other-keys
         )
       ➜
       [➜empty]
       ))
  )

;;--------------------------------------------------------------------------------
;; status-tm definitions
;;
  ;; an empty machine is already parked
  (defun-typed park ((tm status-empty) &optional ➜)
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

  (defun-typed esr ((tm status-empty) &optional ➜)
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

  (defun-typed esw ((tm status-empty) instance &optional ➜)
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

  (def-empty-1 cue-leftmost)

  (defun-typed s ((tm status-empty) &optional ➜)
    (declare (ignore tm))
    (destructering-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  ;; adding a cell to an empty machine as the same as adding a cell to 
  ;; leftmost from an empty machine
  ;; specialized types may depend on this synonym being present, and thus not
  ;; implement their own #'a
  ;;
    (defun-typed a ((tm status-empty) instance &optional ➜)
      (a◧ tm instance ➜)
      )

  (defun-typed on-leftmost ((tm status-empty) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜∅        (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

  (defun-typed on-rightmost ((tm status-empty) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜∅        (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

  (defun-typed tape-length-is-one ((tm status-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))
      
  (defun-typed tape-length-is-two ((tm status-active) &optional ➜)
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
  (def-empty-1 cue-rightmost)
  
  ;; adding a cell to an empty machine will cause it to be parked, then stepping
  ;; one to the right will cause it to be active with the head on leftmost
  ;;
    (defun-typed as ((tm status-empty) instance &optional ➜)
      (destructuring-bind
        (&key
          (➜ok (be t))
          &allow-other-keys
          )
        ➜
        (w (base tm) instance)
        (change-class tm 'status-active)
        [➜ok]
        ))

  ;; we specify these so that we won't lose benefit from the contract
  ;; it is impossible for the head to be on rightmost for an empty machine
  ;; so the user broke his contract with us if he/she calls one of these
  ;; seems we should punish the user for that .. but that sounds hard to do
  ;;
    (defun-typed a&h◨ ((tm status-empty) instance &optional ➜)
      (a tm instance ➜)
      )

    (defun-typed as&h◨ ((tm status-empty) instance &optional ➜)
      (as tm instance ➜)
      )


;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (defun-typed a◧ ((tm status-empty) instance &optional ➜) 
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (w (base tm) instance)
      (change-class tm 'status-parked)
      [➜ok]
      ))
        
  (defun-typed d ((tm status-empty) &optional spill ➜)
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

  (defun-typed d◧ ((tm status-empty) &optional spill ➜)
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

;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;   if a machine in an entanglement group is empty, then all machines
;;   in the entanglement group are empty.  An empty machine is considered to
;;   to have a parked head, so there is a 'heads-on-same-cell' semantic.
;;
  (defun-typed heads-on-same-cell 
    (
      (tm0 status-empty)
      (tm1 status-empty)
      &optional ➜
      )
    (entangled tm0 tm1 ➜)
    )
 
  (defun-typed heads-on-same-cell
    (
      (tm0 status-empty)
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
      (tm1 status-empty)
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

  ;; tm0 and tm1 are entangled, thus are of the same type
  ;; tm0 empty <=> tm1 empty
  ;; tm0 abandoned <=> tm1 abandoned
  ;; tm0 parked, tm1 can be active, vice versa
  (defun-typed s≠ 
    (
      (tm0 status-empty)
      (tm1 status-empty)
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

  (defun-typed a◨ ((tm status-empty) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (w (base tm) instance)
      (change-class tm 'status-active)
      [➜ok]
      ))
