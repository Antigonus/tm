#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; making list machines from other instances
;;
  (defun-typed init 
    (
      (tm list-ea-tm)
      (keyed-parms cons)
      &optional
      (cont-ok #'echo)
      (cont-fail (λ()(error 'bad-info)))
      &rest ⋯
      )
    (setf (entanglements tm) (mk 'list-solo-tm {:init {tm}})) ; initially entangled only with self
    (apply #'call-next-method {tm keyed-parms cont-ok cont-fail (o ⋯)}) ; falls to init for tm-list
    )

  (defun-typed init 
    (
      (tm1 list-ea-tm)
      (tm0 list-ea-tm) ; make an entangled copy of tm0
      &optional
      (cont-ok #'echo)
      (cont-fail (λ()(error 'bad-tm0)))
      &rest ⋯
      )
    (setf (entanglements tm1) (entanglements tm0))
    (let(
          (ents (entanglements tm1))
          )
      (cue-leftmost ents)
      (∃ ents ;; if tm1 does not exist in the entanglements list, add it
        (λ(ents ct c∅)(if (eq (r ents) tm1) [ct] [c∅]))
        #'do-nothing
        (λ()(a&h◨ ents tm1 #'do-nothing #'alloc-fail))
        )
      (apply #'call-next-method {tm1 tm0 cont-ok cont-fail (o ⋯)}) ; falls to list-nd-tm entangled copy
      ))
    
