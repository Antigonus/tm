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
  (def-type status-tr (identity-tr)
    (
      (status
        :initarg status
        :accessor status
        )))

  ;; need to fix this -- when another machine is provided then we are 'active
  ;; if no machine is provided then we are 'empty
  ;; add function to clear the machine (make it empty)
  ;; add function to abandon the machine
  (defun-typed init 
    (
      (tm status-tr)
      (init-value tape-machine)
      &optional
      (cont-ok #'echo)
      (cont-fail (λ()(error 'bad-init-value)))
      &rest ⋯
      )
    (declare (ignore ⋯ cont-fail))
    (setf (status tm) 'empty)
    (call-next-method)
    (funcall cont-ok tm)
    )

   (defun is-legal-status (status)
     (case status
       (('abandoned 'empty 'parked 'active) t)
       (otherwise ∅)
       ))

;;--------------------------------------------------------------------------------
;; tm-decl-only
;;
  (defun-typed r ((tm status-tr) &rest ⋯)
    (if 
      (eq (status tm) 'active)
      (apply #'r (cons (base tm) ⋯))
      (destructuring-bind
        (
          &key
          (cont-abandoned #'operation-on-abandoned)
          (cont-empty #'use-of-empty)
          (cont-parked #'parked-head-use)
          )
        ⋯
        (case (status tm)
          (('abandoned) [cont-abandoned])
          (('empty) [cont-empty])
          (('parked) [cont-parked])
          (otherwise  (cant-happen))
          ))))

