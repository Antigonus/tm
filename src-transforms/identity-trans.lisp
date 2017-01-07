#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Base class for transforms. Library users never see this.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a tape machine
;;
  (defclass identity-trans (abstract-tape-machine)
    (
      (base ; the machine we transform
        :initarg base
        :accessor base
        )))

;;--------------------------------------------------------------------------------
;; making transform machines
;;
  (defmethod init 
    (
      (tm identity-trans)
      (init-value abstract-tape-machine)
      &optional
      (cont-ok #'echo)
      (cont-fail (λ()(error 'bad-init-value)))
      &rest ⋯
      )
    (declare (ignore ⋯ cont-fail))
    (setf (base tm) init-value)
    (funcall cont-ok tm)
    )

define macro that takes a function name, then passes all args through to the
same function call on the base machine

oh fun, we get to list all of the functions in the system, and if we get it wrong,
or something is edited, then all goes wrong ... wish the function name could be a
parameter too


