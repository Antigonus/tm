#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Wraps another tape machine so as to provide and use status information 
about the lower machine.  Status is either 'empty, 'parked, 'active.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a tape machine
;;
  (defclass uber-trans (identity-trans)
    (
      (status
        :initarg status
        :accessor status
        )))

  (defmethod init 
    (
      (tm uber-trans)
      (init-value abstract-tape-machine)
      &optional
      (cont-ok #'echo)
      (cont-fail (λ()(error 'bad-init-value)))
      &rest ⋯
      )
    (declare (ignore ⋯ cont-fail))
    (setf (status tm) 'empty)
    (funcall cont-ok tm)
    )

