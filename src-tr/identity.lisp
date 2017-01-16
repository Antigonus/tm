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
  (defun-typed r ((tm identity-tr) &rest ⋯) (apply #'r (cons (base tm) ⋯)))
  (defun-typed esr ((tm identity-tr) &rest ⋯) (apply #'esr (cons (base tm) ⋯)))

  (defun-typed w ((tm identity-tr) &rest ⋯) (apply #'w (cons (base tm) ⋯)))
  (defun-typed esw ((tm identity-tr) &rest ⋯) (apply #'esw (cons (base tm) ⋯)))

  (defun-typed cue-leftmost ((tm identity-tr) &rest ⋯) (apply #'cue-leftmost (cons (base tm) ⋯)))

  (defun-typed s ((tm identity-tr) &rest ⋯) (apply #'s (cons (base tm) ⋯)))
  (defun-typed a ((tm identity-tr) &rest ⋯) (apply #'a (cons (base tm) ⋯)))

  (defun-typed on-leftmost ((tm identity-tr) &rest ⋯) (apply #'on-leftmost (cons (base tm) ⋯)))
  (defun-typed on-rightmost ((tm identity-tr) &rest ⋯) (apply #'on-rightmost (cons (base tm) ⋯)))

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (defun-typed a◧ ((tm identity-tr) &rest ⋯) (apply #'a◧ (cons (base tm) ⋯)))
  (defun-typed d ((tm identity-tr) &rest ⋯) (apply #'d (cons (base tm) ⋯)))
  (defun-typed d◧ ((tm identity-tr) &rest ⋯) (apply #'d◧ (cons (base tm) ⋯)))

;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
  (defun-typed heads-on-same-cell 
    ((tm0 identity-tr) (tm1 identity-tr) &rest ⋯)
    (apply #'heads-on-same-cell (cons (base tm0) (cons (base tm1) ⋯)))
    )
  (defun-typed heads-on-same-cell 
    ((tm0 identity-tr) (tm1 tape-machine) &rest ⋯)
    (apply #'heads-on-same-cell (cons (base tm0) (cons tm1 ⋯)))
    )
  (defun-typed heads-on-same-cell 
    ((tm0 tape-machine) (tm1 identity-tr) &rest ⋯)
    (apply #'heads-on-same-cell (cons tm0 (cons (base tm1) ⋯)))
    )

