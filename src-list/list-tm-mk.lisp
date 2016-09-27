#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; making list machines from other objects
;;
  (defmethod init 
    (
      (tm list-tm)
      init-list 
      &optional
      (cont-ok #'echo)
      (cont-fail (λ()(error 'bad-init-value)))
      &rest ⋯
      )
    (declare (ignore ⋯))
    (destructuring-bind
      (&key mount &allow-other-keys) init-list
      (cond
        ((∧ mount (consp mount))
          (setf (HA tm) mount)
          (setf (tape tm) mount)
          (funcall cont-ok tm)
          )
        (t
          (funcall cont-fail)
          ))))
    
  (defmethod mount
    (
      (sequence cons)
      &optional 
      (cont-ok #'echo)
      (cont-fail (λ()(error 'mount-unrecognized-sequence-type)))
      (cont-no-alloc #'alloc-fail)
      )
    (mk 'list-tm {:mount sequence} cont-ok cont-fail cont-no-alloc)
    )

