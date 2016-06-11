#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  This machine's tape is woven in a depth first pattern through
  the base tm interpreted as a tree.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-depth (tape-machine)())

  (defstruct tm-depth-parms
    base ; machine being traversed
    history ; backtrack buffer
    )

  ;; base is another tape machine
  (defmethod init 
    (
      (tm tm-depth)
      init-list
      &optional 
      (cont-ok (be t))
      (cont-fail (λ()(error 'bad-init-value)))
      )
    (destructuring-bind
      (&key base &allow-other-keys) init-list
      (if 
        base
        (progn
          (setf (HA tm) (make-instance 'stack))
          (setf (tape tm) base)
          (setf (parameters tm) ∅)
          (setf (entanglements tm) (make-entanglements tm))
          (funcall cont-ok)
          )
        (funcall cont-fail)
        )))
