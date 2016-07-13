#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defmethod r ((tm list-tm) &rest ⋯)
    (declare (ignore ⋯))
    (car (HA tm))
    )

  (defmethod w ((tm list-tm) object &rest ⋯)
    (declare (ignore ⋯))
    (setf (car (HA tm)) object)
    t
    )

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defmethod cue-leftmost ((tm list-tm) &rest ⋯)
    (declare (ignore ⋯)) 
    (setf (HA tm) (tape tm))
    t
    )

;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defmethod s
    (
      (tm list-tm)
      &optional 
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (if 
      (cdr (HA tm))
      (progn
        (setf (HA tm) (cdr (HA tm)))
        (funcall cont-ok)
        )
      (funcall cont-rightmost)
      ))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defmethod a
    (
      (tm list-tm)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (be ∅))
      )
    (declare (ignore cont-no-alloc))
    (let(
          (next-cell (cdr (HA tm)))
          )
      (rplacd (HA tm) (cons object next-cell))
      (funcall cont-ok)
      ))
