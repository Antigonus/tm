#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt
  
|#
(in-package #:tm)

(defun empty (tm &optional (cont-true (be t)) (cont-false (be ∅)))
  (empty-0 tm (state tm) cont-true cont-false)
  )
(defgeneric empty-0 (tm state cont-true cont-false))
(defmethod empty-0
  (
    (tm tape-machine)
    (state void) 
    cont-true 
    cont-false
    )
  (declare (ignore tm state cont-false))
  (funcall cont-true)
  )
(defmethod empty-0
  (
    (tm tape-machine)
    (state parked)
    cont-true 
    cont-false
    )
  (declare (ignore tm state cont-true))
  (funcall cont-false)
  )
(defmethod empty-0
  (
    (tm tape-machine)
    (state active)
    cont-true 
    cont-false
    )
  (declare (ignore tm state cont-true))
  (funcall cont-false)
  )

