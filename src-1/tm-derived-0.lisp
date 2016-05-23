#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These functions derive the remainder of the tape-machine interface while using only the
primitives from tm-primitives.  

There is no functional need for a new tape machine implementation to specialize these
functions.  Still, some implementations will want to specialize these functions for
performance reasons.

Because these are built upon the primitives, they can only be tested against implementations
of the primitives.


|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; tape-machine duplication
;;   we need a layer 0 with no entanglement accounting in order to implement the
;;   entanglement list functions sans circular references.
;;
  ;; cue-to-0 is primitive

  (defun dup-1 (tm-orig)
    "Adds entanglement accounting to cue-to-0 result, but leaves out the tm-cued
     machine.
    "
    (let(
          (tm-cued (make-instance (type-of tm-orig)))
          )
      (cue-to-0 tm-cued tm-orig)
      (setf (entanglements tm-cued) (entanglements tm-orig))
      tm-cued
      ))

  (defun dup-0 (tm-orig)
    "Creates a new machines that is a copy of another, sans entanglement accounting."
    (let(
          (tm-cued (make-instance (type-of tm-orig)))
          )
      (cue-to-0 tm-cued tm-orig)
      (setf (entanglements tm-cued) ∅)
      tm-cued
      ))

;;--------------------------------------------------------------------------------
;; leftmost read and write
;;
  (defun r◧
    (
      tm
      &optional
      (cont-ok #'echo)
      (cont-void (λ()(error 'access-void)))
      )
    "read leftmost cell of the tape"
    (r◧-0 tm (state tm) cont-ok cont-void)
    )

  (defgeneric r◧-0 (tm state cont-ok cont-void))
  (defmethod r◧-0  (tm (state void) cont-ok cont-void)
    (declare (ignore tm state cont-ok))
    (funcall cont-void)
    )
  (defmethod r◧-0  (tm state cont-ok cont-void)
    (declare (ignore cont-void))
    (let(
          (tm1 (dup-0 tm))
          )
      (cue-leftmost tm1)
      (r tm1 cont-ok #'cant-happen) ; cue-leftmost would have unparked the head
      ))

  (defun w◧
    (
      tm
      &optional
      (cont-ok #'echo)
      (cont-void (λ()(error 'access-void)))
      )
    "read leftmost cell of the tape"
    (w◧-0 tm (state tm) cont-ok cont-void)
    )

  (defgeneric w◧-0 (tm state cont-ok cont-void))
  (defmethod w◧-0  (tm (state void) cont-ok cont-void)
    (declare (ignore tm state cont-ok))
    (funcall cont-void)
    )
  (defmethod w◧-0  (tm state cont-ok cont-void)
    (declare (ignore cont-void))
    (let(
          (tm1 (dup-0 tm))
          )
      (cue-leftmost tm1)
      (w tm1 cont-ok #'cant-happen) ; cue-leftmost would have unparked the head
      ))


;;--------------------------------------------------------------------------------
;; cueing
;;  
  (defun cue-rightmost (tm &optional (cont-ok (be t)) (cont-void (be ∅)))
    "Cue tm's head to the rightmost cell."
    (cue-rightmost-0 tm (state tm) cont-ok cont-void)
    )
  (defgeneric cue-rightmost-0 (tm state cont-ok cont-void))
  (defmethod cue-rightmost-0 (tm (state void) cont-ok cont-void)
    (declare (ignore tm state cont-ok))
    (funcall cont-void)
    )
  (defmethod cue-rightmost-0 (tm (state parked) cont-ok cont-void)
    (declare (ignore state cont-void))
    (cue-leftmost tm) ; this unparks the head
    (cue-rightmost tm)
    (funcall cont-ok)
    )
  ;; do not want to depend on quantifiers, so I built the loop
  ;; should throw a computationally complex warning
  (defmethod cue-rightmost-0 (tm (state active) cont-ok cont-void)
    (declare (ignore state cont-void))
    (labels(
             (work() (s tm #'work cont-ok))
             )
      (work)
      ))

;;--------------------------------------------------------------------------------
;; These are primitives for the generic implementation of location.  They must have
;; specializations.
;;  
  (defun on-leftmost 
    (
      tm
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    "tm head is on the leftmost cell."
    (on-leftmost-0 tm cont-true cont-false)
    )
  (defgeneric on-leftmost-0 (tm cont-true cont-false))
  (defmethod on-leftmost-0 (tm cont-true cont-false)
    (let(
          (tm1 (dup-0 tm))
          )
      (cue-leftmost tm1)
      (heads-on-same-cell tm1 tm cont-true cont-false)
      ))

  (defun on-rightmost
    (tm
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    "tm head is on the rightmost cell."
    (on-rightmost-0 tm cont-true cont-false)
    )
  (defgeneric on-rightmost-0 (tm cont-true cont-false))
  (defmethod on-rightmost-0 (tm cont-true cont-false)
    (let(
          (tm1 (dup-0 tm))
          )
      (s tm1 cont-false cont-true)
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
    "step tm0 unless it is equal to tm1"
    (s≠-0 tm0 (state tm0) tm1 (state tm1) cont-ok cont-rightmost cont-bound)
    )
  (defgeneric s≠-0 (tm0 state0 tm1 state1 cont-ok cont-rightmost cont-bound))
  (defmethod s≠-0 (tm0 state0 tm1 state1 cont-ok cont-rightmost cont-bound)
    (declare (ignore state0 state1))
    (cond
      ((heads-on-same-cell tm0 tm1) (funcall cont-bound))
      (t
        (s tm0 cont-ok cont-rightmost)
      )))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
;; Allocated cells must be initialized.  The initialization value is provided
;; directly or though a fill machine.
;;
  (defun as
    (
      tm tape-machine
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "Like #'a, but tm is stepped to the new cell"
    (as-0 tm object cont-ok cont-no-alloc)
    )
  (defgeneric as-0 (tm state object cont-ok cont-no-alloc))
  (defmethod as-0 (tm state object cont-ok cont-no-alloc)
    (a tm object 
      (λ()(s tm cont-ok #'cant-happen))
      cont-no-alloc
      ))

  (defgeneric a&h◨ (tm object &optional cont-ok cont-no-alloc)
    (:documentation 
      "#'a with a contract that the head is on rightmost.
       Some implementatons will be able to specialize this and make it more efficient.
      "))

  (defmethod a&h◨
    (
      (tm tape-machine)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (a tm object (λ()(s tm)(funcall cont-ok)) cont-no-alloc)
    )

  (defgeneric a&h◨s (tm object &optional cont-ok cont-no-alloc)
    (:documentation 
      "#'as with a contract that the head is on rightmost.
       Some implementatons will be able to specialize this and make it more efficient.
      "))

  (defmethod a&h◨s
    (
      (tm tape-machine)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (as tm object cont-ok cont-no-alloc)
    )


