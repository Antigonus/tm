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

  ;; the init-value as a cons cell falls back to list-tm version.

  ;; This makes an entangled machine.  It is safe to do so as nd machines
  ;; do not support destructive operations.
  ;;
    (defmethod init 
      (
        (tm list-nd-tm)
        (init-value list-nd-tm)
        &optional
        (cont-ok #'echo)
        (cont-fail (λ()(error 'bad-init-value)))
        &rest ⋯
        )
      (declare (ignore ⋯ cont-fail))
      (setf (HA tm) (HA init-value))
      (setf (tape tm) (tape init-value))
      (funcall cont-ok tm)
      )
    
