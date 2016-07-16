#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  non-destructive operation primitives

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; copying
;;  
  (defmethod mk-entangled
    (
      (tm-orig list-nd-tm)
      )
    (let(
          (tm1 (make-instance 'list-nd-tm))
          )
      (setf (HA tm1) (HA tm-orig))
      (setf (tape tm1) (tape tm-orig))
      tm1
      ))

  (defmethod recycle-entangled
    (
      (tm-orig list-nd-tm)
      tm-to-be-recycled 
      )
    (change-class tm-to-be-recycled (type-of tm-orig))
    (setf (HA tm-to-be-recycled) (HA tm-orig))
    (setf (tape tm-to-be-recycled) (tape tm-orig))
    tm-to-be-recycled
    )


;;--------------------------------------------------------------------------------
;; head location
;;
  (defmethod heads-on-same-cell
    (
      (tm0 list-nd-tm)
      (tm1 list-nd-tm)
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
      (tm0 list-nd-tm)
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
      (tm1 list-nd-tm)
      &optional
      cont-true
      cont-false
      &rest ⋯
      )
    (declare (ignore cont-true ⋯)) 
    (funcall cont-false)
    )
