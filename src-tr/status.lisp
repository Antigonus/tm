#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Wraps another tape machine so as to provide and use status information 
about the lower machine.  Status is either 'empty, 'parked, 'active.

If we include 'abandoned as a status, then the entire interface will
have to be reproduced here, and inheriting from identity does nothing.
But the same can be said of 'empty. So we may as well keep abandoned.
.. well at least we get to inherit base from identity..

base machines must have an initializer that accepts an :init keyword
status machines do not have an :init initializer keyword.

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
        )
      (type
        :initarg type
        :accessor type
        )))


;;--------------------------------------------------------------------------------
;; status
;;
  (defun is-legal-status (status &optional (ct (be t)) (c∅ (be ∅)))
    (case status
      (('abandoned 'empty 'parked 'active) [ct])
      (otherwise [c∅])
      ))


;;--------------------------------------------------------------------------------
;; making status transforms
;;
  (defun-typed init 
    (
      (tm status-tr)
      (init-value cons)
      &optional
      (cont-ok #'echo)
      (cont-fail (λ()(error 'bad-init-value)))
      &rest ⋯
      )
    (declare (ignore ⋯))
    (destructuring-bind
      (&key base type) init-value
      (cond
        (base
          (setf (base tm) base)
          (setf (type tm) (type-of base))
          (setf (status tm) 'active)
          [cont-ok tm]
          )
        ((∧ base type) [cont-fail])
        (type
          (setf (type tm) type)
          (setf (status tm) 'empty)
          [cont-ok tm]
          )
        (t [cont-fail])
        )))

  (defun-typed init 
    (
      (tm status-tr)
      (init-value status-tr)
      &optional
      (cont-ok #'echo)
      (cont-fail (λ()(error 'bad-init-value)))
      &rest ⋯
      )
    (declare (ignore ⋯ cont-fail))
    (case (status init-value)
      ('abandoned
        [cont-fail]
        )
      (('active 'parked)
        (setf (base tm) (mk (type-of (base tm)) tm)) ; makes an entangled copy
        (setf (type tm) (type init-value))
        (setf (status tm) (status init-value))
        [cont-ok tm]
        )
      ('empty
        (setf (type tm) (type init-value))
        (setf (status tm) 'empty)
        [cont-ok tm]
        )
      ))


;;--------------------------------------------------------------------------------
;; tm-decl-only
;;
  (defun-typed r ((tm status-tr) cont-ok &rest ⋯)
    (if 
      (eq (status tm) 'active)
      (apply #'r {(base tm) cont-ok (o ⋯)})
      (destructuring-bind
        (
          &key
          (cont-abandoned #'operation-on-abandoned)
          (cont-empty     #'use-of-empty)
          (cont-parked    #'parked-head-use)
          )
        ⋯
        (case (status tm)
          ('abandoned [cont-abandoned])
          ('empty     [cont-empty])
          ('parked    [cont-parked])
          (otherwise  [cant-happen])
          ))))

  ;; see notes in docs, status.txt
  (defun-typed esr
    (
      (tm identity-tr)
      &optional 
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'step-from-rightmost)))
      &rest ⋯
      )
    (if 
      (eq (status tm) 'active)
      (apply #'esr {(base tm) cont-ok cont-rightmost (o ⋯)})
      (destructuring-bind
        (
          &key
          (cont-abandoned #'operation-on-abandoned)
          )
        ⋯
        (case (status tm)
          ('abandoned [cont-abandoned])
          ('empty     [cont-rightmost])
          ('parked    (apply #'r {(base tm) cont-ok (o ⋯)}))
          (otherwise  [cant-happen])
          ))))

  (defun-typed esw
    (
      (tm status-tr)
      instance
      &optional 
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      &rest ⋯
      )
    (if 
      (eq (status tm) 'active)
      (apply #'esw {(base tm) instance cont-ok cont-rightmost (o ⋯)})
      (destructuring-bind
        (
          &key
          (cont-abandoned #'operation-on-abandoned)
          )
        ⋯
        (case (status tm)
          ('abandoned [cont-abandoned])
          ('empty  
            (setf (base tm) (mk (type tm) {:init {instance}}))
            )
          (('parked) ; when parked, the base machine head is on leftmost
            (apply #'w {(base tm) instance cont-ok (o ⋯)})
            )
          (otherwise  [cant-happen])
          ))))

