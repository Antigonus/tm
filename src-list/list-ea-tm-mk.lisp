#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; making list machines from other objects
;;   my gosh this is an expensive way to make a temporary variable ..
;;
  (defmethod init 
    (
      (tm list-ea-tm)
      (init-value cons)
      &optional
      (cont-ok #'echo)
      (cont-fail (λ()(error 'bad-init-value)))
      &rest ⋯
      )
    (declare (ignore ⋯ cont-fail))
    (setf (HA tm) init-value)
    (setf (tape tm) init-value)
    (setf (entanglements tm) (mk 'list-solo-tm {tm})) ; initially entangled only with self
    (funcall cont-ok tm)
    )

  (defmethod init 
    (
      (tm list-ea-tm)
      (init-value list-ea-tm)
      &optional
      (cont-ok #'echo)
      (cont-fail (λ()(error 'bad-init-value)))
      &rest ⋯
      )
    (declare (ignore ⋯ cont-fail))
    (setf (HA tm) (HA init-value))
    (setf (tape tm) (tape init-value))
    (setf (entanglements tm) (entanglements init-value))
    (let(
          (etms (entanglements tm))
          )
      (cue-leftmost etms)
      (¬∃ etms
        (λ(etms)(eq (r etms) tm))
        (λ()(a&h◨ etms tm #'do-nothing #'alloc-fail))
        #'do-nothing
        )
      (funcall cont-ok tm)
      ))
    
