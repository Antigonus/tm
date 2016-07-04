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
;; tape-machine copying
;;   we need a layer 0 with no entanglement accounting in order to implement the
;;   entanglement list functions sans circular references.
;;
  ;; cue-to-0 is primitive

  (defun fork-1 (tm-orig)
    "Returns a machine that is a fork of another, but the returned machine
     does not appear in its own entanglements list.
     "
    (let(
          (tm-cued (make-instance (type-of tm-orig)))
          )
      (cue-to-0 tm-cued tm-orig)
      (setf (entanglements tm-cued) (entanglements tm-orig))
      tm-cued ; tm-cued does not appear in its own entanglements list
      ))

  (defun fork-0 (tm-orig)
    "Returns a machine that is a fork of another, but without entanglement accounting.
    "
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
          (tm1 (fork-0 tm))
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
          (tm1 (fork-0 tm))
          )
      (cue-leftmost tm1)
      (w tm1 cont-ok #'cant-happen) ; cue-leftmost would have unparked the head
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
    (on-leftmost-0 tm (state tm) cont-true cont-false)
    )
  (defgeneric on-leftmost-0 (tm state cont-true cont-false))
  (defmethod on-leftmost-0 (tm (state void) cont-true cont-false)
    (declare (ignore tm state cont-true))
    (funcall cont-false)
    )
  (defmethod on-leftmost-0 (tm (state parked) cont-true cont-false)
    (declare (ignore tm state cont-true))
    (funcall cont-false)
    )
  (defmethod on-leftmost-0 (tm (state active) cont-true cont-false)
    (let(
          (tm1 (fork-0 tm))
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
          (tm1 (fork-0 tm))
          )
      (s tm1 cont-false cont-true)
      ))



