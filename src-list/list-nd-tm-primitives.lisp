#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  non-destructive operation primitives

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; head location
;;
;;

  ;; Though entangled copy function is not directly called in heads-on-same-cell, a copy
  ;; is implied because the function accepts two state machines that share a tape, tm0 and
  ;; tm1.
  (defmethod heads-on-same-cell
    (
      (tm0 list-nd-tm)
      (tm1 list-nd-tm)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      &rest ⋯
      )
    (declare (ignore ⋯)) 
    (if (eq (HA tm0) (HA tm1))
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defmethod heads-on-same-cell
    (
      (tm0 list-nd-tm)
      tm1
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      &rest ⋯
      )
    (declare (ignore cont-true ⋯)) 
    (funcall cont-false)
    )

  (defmethod heads-on-same-cell
    (
      tm0
      (tm1 list-nd-tm)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      &rest ⋯
      )
    (declare (ignore cont-true ⋯)) 
    (funcall cont-false)
    )
