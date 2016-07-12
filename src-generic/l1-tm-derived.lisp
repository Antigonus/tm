#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These functions are derived just from the primitives, so there is no functional need for a
new tape machine implementation to specialize them.


|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; cueing
;;  
  (defun cue-rightmost (tm &optional (cont-ok (be t)) (cont-void (be ∅)))
    "Cue tm's head to the rightmost cell."
    (cue-rightmost-0 tm (state tm) cont-ok cont-void)
    )
  (defgeneric cue-rightmost-0 (tm state cont-ok cont-void))
  (defmethod cue-rightmost-0 ((tm tape-machine)(state void) cont-ok cont-void)
    (declare (ignore tm state cont-ok))
    (funcall cont-void)
    )
  (defmethod cue-rightmost-0 ((tm tape-machine) (state parked) cont-ok cont-void)
    (declare (ignore state cont-void))
    (cue-leftmost-0 tm parked
      (λ()(cue-rightmost-0 tm active cont-ok #'cant-happen))
      #'cant-happen
      ))

  ;; step does not move forward from rightmost, rather takes the rightmost continuation
  ;; Do not want to depend on quantifiers, so I built the loop.
  ;; Might be that we should throw a computationally complex warning
  (defmethod cue-rightmost-0 ((tm tape-machine) (state active) cont-ok cont-void)
    (declare (ignore state cont-void))
    (labels(
             (work() (s tm #'work cont-ok))
             )
      (work)
      ))

;;--------------------------------------------------------------------------------
;; stepping with a boundary, boundaries are inclusive
;;
  (defun s≠ 
    (
      tm0
      tm1
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      (cont-bound (be ∅))
      )
    "tm0 and tm1 are on the same tape.  Step tm0 unless it is equal to tm1."
    (s≠-0 tm0 (state tm0) tm1 (state tm1) cont-ok cont-rightmost cont-bound)
    )
  (defgeneric s≠-0 (tm0 state0 tm1 state1 cont-ok cont-rightmost cont-bound))
  (defmethod s≠-0
    (
      (tm0 tape-machine)
      (state0 void)
      (tm1 tape-machine)
      (state1 void) 
      cont-ok
      cont-rightmost 
      cont-bound
      )
    (declare (ignore tm0 state0 tm1 state1 cont-ok cont-rightmost))
    (funcall cont-bound)
    )
  (defmethod s≠-0 
    (
      (tm0 tape-machine)
      (state0 void)
      (tm1  tape-machine)
      state1 
      cont-ok
      cont-rightmost 
      cont-bound
      )
    (declare (ignore tm0 state0 tm1 state1 cont-ok cont-bound))
    (funcall cont-rightmost)
    )
  (defmethod s≠-0
    (
      (tm0 tape-machine)
      state0
      (tm1 tape-machine)
      (state1 void) 
      cont-ok
      cont-rightmost 
      cont-bound
      )
    (declare (ignore state0 tm1 state1 cont-bound))
    (s tm0 cont-ok cont-rightmost)
    )
  (defmethod s≠-0 
    (
      (tm0 tape-machine)
      state0 
      (tm1 tape-machine)
      state1
      cont-ok 
      cont-rightmost
      cont-bound
      )
    (declare (ignore state0 state1))
    (heads-on-same-cell tm0 tm1
      cont-bound
      (λ()(s tm0 cont-ok cont-rightmost))
      #'cant-happen
      ))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
;; Allocated cells must be initialized.  The initialization value is provided
;; directly or though a fill machine.
;;
  (defun as
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "Like #'a, but tm is stepped to the new cell"
    (a tm object 
      (λ()(s tm cont-ok #'cant-happen))
      cont-no-alloc
      ))

  ;; append with contract that head is at rightmost
  (defun a&h◨ 
    (
      tm 
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "#'a with a contract that the head is on rightmost."
    (a&h◨-0 tm object cont-ok cont-no-alloc)
    )
  (defgeneric a&h◨-0 (tm object cont-ok cont-no-alloc))
  ;; some specializations can make better use of this contract
  (defmethod a&h◨-0 ((tm tape-machine) object cont-ok cont-no-alloc)
    (a tm object (λ()(s tm)(funcall cont-ok)) cont-no-alloc)
    )

  ;; append, and head is at rightmost, then step
  (defun a&h◨s
    (
      tm 
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "#'as with a contract that the head is on rightmost."
    (a&h◨-0 tm object cont-ok cont-no-alloc)
    )
  (defgeneric a&h◨s-0 (tm object cont-ok cont-no-alloc))
  ;; some specializations can make better use of this contract
  (defmethod a&h◨s-0 ((tm tape-machine) object cont-ok cont-no-alloc)
    (as tm object (λ()(s tm)(funcall cont-ok)) cont-no-alloc)
    )



