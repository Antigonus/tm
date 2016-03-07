#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  tm-subspace is yet another window into another tape machine.

  In this case read, write, allocate, etc, just pass through to the
  base machine.  However, our leftmost and rightmost may be different
  than the leftmost and rightmost of the base machine.

  Interesting, a subspace on a line, is an interval.  Interesting
  metaprogrammign potential here.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-subspace (tape-machine)())

  (defstruct subspace
    leftmost ; a tape machine with head on the subspace leftmost
    rightmost ; a tape machine *on the same tape* with head on the new rightmost
    )

  (defun mk-tm-subspace 
    (
      &optional
      init
      (cont-ok #'echo) 
      (cont-fail (λ() (error 'tm-mk-bad-init-type :text "expected subspace struct")))
      )
    (let(
          (tm (make-instance 'tm-subspace))
          )
      (unless
        (∧
          init
          (typep init 'subspace)
          )
        (funcall cont-fail)
        )

      (setf (HA tm) (dup (subspace-leftmost init))) ; HA is a dup of the leftmost machine
      (setf (tape tm) init)
      (funcall cont-ok tm)
      ))

  (mk-tm-hook 'tm-subspace #'mk-tm-subspace)

;;--------------------------------------------------------------------------------
;; essential methods
;;
  (defmethod r ((tm tm-subspace))
    (r (HA tm))
    )

  (defmethod w ((tm tm-subspace) object)
    (w (HA tm) object)
    t
    )
 
  (defmethod cue-leftmost  ((tm tm-subspace)) 
    (cue-to (HA tm) (subspace-leftmost (tape tm)))
    t
    )

  ;; subspaces may be nested
  (defun heads-on-same-cell-subspace (tm0 tm1 cont-true cont-false)
    (let(
          (base-0 tm0)
          (base-1 tm1)
          )
      (loop
        (unless (typep base-0 'tm-subspace) (return))
        (setf base-0 (HA base-0))
        )
      (loop
        (unless (typep base-1 'tm-subspace) (return))
        (setf base-1 (HA base-1))
        )
      (heads-on-same-cell base-0 base-1 cont-true cont-false)
      ))


  (defmethod heads-on-same-cell 
    (
      (tm0 tm-subspace) 
      (tm1 tape-machine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-subspace tm0 tm1 cont-true cont-false)
    )

  (defmethod heads-on-same-cell 
    (
      (tm0 tape-machine) 
      (tm1 tm-subspace) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-subspace tm0 tm1 cont-true cont-false)
    )

  (defmethod s
    (
      (tm tm-subspace)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (s (HA tm) cont-ok cont-rightmost)
    )

  ;; allocate a cell
  (defmethod a
    (
      (tm tm-subspace)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (error 'tm-alloc-fail))
      )
    (a (HA tm) cont-ok cont-no-alloc)
    )

  (defmethod d 
    (
      (tm tm-subspace)
      &optional 
      spill
      (cont-ok (be t))
      (cont-rightmost (λ()(error 'tm-deallocation-request-at-rightmost)))
      (cont-no-alloc (error 'tm-alloc-fail))
      )
    (if
      (heads-on-same-cell (HA tm) (subspace-rightmost (tape tm)))
      (funcall cont-rightmost)
      (d (HA tm) spill cont-ok cont-rightmost cont-no-alloc)
      ))


    
