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
  (def-type identity-tr (tape-machine)
    (
      (base ; the machine we transform
        :initarg base
        :accessor base
        )))

;;--------------------------------------------------------------------------------
;; making transform machines
;;
  (defun-typed init 
    (
      (tm identity-tr)
      (init-value tape-machine)
      &optional
      (cont-ok #'echo)
      (cont-fail (λ()(error 'bad-init-value)))
      &rest ⋯
      )
    (declare (ignore ⋯ cont-fail))
    (setf (base tm) init-value)
    (funcall cont-ok tm)
    )

;;--------------------------------------------------------------------------------
;;tm-decl-only
;;
  (defun-typed r ((tm identity-tr) &rest ⋯)
    (apply #'r {(base tm) (o ⋯)})
    )
  (defun-typed esr
    (
      (tm identity-tr)
      &optional 
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'step-from-rightmost)))
      &rest ⋯
      )
    (apply #'esr {(base tm) cont-ok cont-rightmost (o ⋯)})
    )

  (defun-typed w ((tm identity-tr) instance &rest ⋯)
    (apply #'w {(base tm) (o ⋯)})
    )
  (defun-typed esw
    (
      (tm identity-tr)
      instance
      &optional 
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      &rest ⋯
      )
    (apply #'esr {(base tm) cont-ok cont-rightmost (o ⋯)})
    )

  (defun-typed cue-leftmost ((tm identity-tr) &rest ⋯)
    (apply #'cue-leftmost {(base tm) (o ⋯)})
    )

  (defun-typed s
    (
      (tm identity-tr)
      &optional 
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      &rest ⋯
      )
    (apply #'s {(base tm) cont-ok cont-rightmost (o ⋯)})
    )

  (defun-typed a
    (
      (tm identity-tr)
      instance
      &optional 
      (cont-ok (be t))
      (cont-no-alloc (be ∅))
      &rest ⋯
      )
    (apply #'s {(base tm) instance cont-ok cont-no-alloc (o ⋯)})
    )

  (defun-typed on-leftmost 
    (
      (tm identity-tr)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      &rest ⋯
      )
    (apply #'on-leftmost {(base tm) cont-true cont-false (o ⋯)})
    )

  (defun-typed on-rightmost
    (
      (tm identity-tr)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      &rest ⋯
      )
    (apply #'on-rightmost {(base tm) cont-true cont-false (o ⋯)})
    )

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (defun-typed a◧
    (
      (tm identity-tr)
      instance
      &optional
      (cont-ok (be t))
      (cont-no-alloc #'alloc-fail)
      &rest ⋯
      )
    (apply #'a◧ {(base tm) instance cont-ok cont-no-alloc (o ⋯)})
    )

  (defun-typed d 
    (
      (tm identity-tr)
      &optional
      spill 
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
      (cont-no-alloc #'alloc-fail)
      (cont-collision #'cant-happen)
      &rest ⋯
      )
    (apply #'d {(base tm) spill cont-ok cont-rightmost cont-no-alloc cont-collision (o ⋯)})
    )

  (defun-typed d◧
    (
      (tm identity-tr)
      &optional
      spill 
      (cont-ok #'echo)
      (cont-no-alloc #'alloc-fail)
      (cont-collision (λ()(error 'dealloc-collision)))
      &rest ⋯
      )
    (apply #'d◧ {(base tm) spill cont-ok cont-no-alloc cont-collision (o ⋯)})
    )


;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
  (defun-typed heads-on-same-cell
    (
      (tm0 identity-tr)
      (tm1 identity-tr)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      &rest ⋯
      )
    (apply #'heads-on-same-cell {(base tm0) (base tm1) cont-true cont-false (o ⋯)})
    )

  (defun-typed heads-on-same-cell
    (
      (tm0 identity-tr)
      (tm1 tape-machine)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      &rest ⋯
      )
    (apply #'heads-on-same-cell {(base tm0) tm1 cont-true cont-false (o ⋯)})
    )

  (defun-typed heads-on-same-cell
    (
      (tm0 tape-machine)
      (tm1 identity-tr)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      &rest ⋯
      )
    (apply #'heads-on-same-cell {tm0 (base tm1) cont-true cont-false (o ⋯)})
    )
