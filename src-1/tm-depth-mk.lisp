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

  ;; base is another tape machine
  (defmethod init 
    (
      (instance tm-depth)
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
          (setf (HA instance) (mk 'stack-list))
          (setf (tape instance) base)
          (funcall cont-ok)
          )
        (funcall cont-fail)
        )))
