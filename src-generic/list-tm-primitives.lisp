#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

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
    )

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defmethod cue-leftmost ((tm list-tm) &rest ⋯)
    (declare (ignore ⋯)) 
    (setf (HA tm) (tape tm))
    )

;;--------------------------------------------------------------------------------
;; head location
;;
  (defmethod heads-on-same-cell
    (
      (tm0 list-tm)
      (tm1 list-tm)
      &optional
      cont-true
      cont-false
      &rest ⋯
      )
    (declare (ignore ⋯)) 
    (if (eq (HA tm0) (HA tm1))
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defmethod heads-on-same-cell
    (
      (tm0 list-tm)
      tm1
      &optional
      cont-true
      cont-false
      &rest ⋯
      )
    (declare (ignore cont-true ⋯)) 
    (funcall cont-false)
    )

  (defmethod heads-on-same-cell
    (
      tm0
      (tm1 list-tm)
      &optional
      cont-true
      cont-false
      &rest ⋯
      )
    (declare (ignore cont-true ⋯)) 
    (funcall cont-false)
    )
    
;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defgeneric s
    (
      (tm list-tm)
      &optional 
      cont-ok
      cont-rightmost
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
      cont-ok
      cont-no-alloc
      )
    (declare (ignore cont-no-alloc))
    (let(
          (next-cell (cdr (HA tm)))
          )
      (setf (HA tm) (cons object next-cell))
      (funcall cont-ok)
      ))
