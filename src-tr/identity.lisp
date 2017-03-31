#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Base class for transforms. Library users probably never see this.

Identity does not implement generic functions, as those don't reduce immediately to
operations on the base, but rather are a composition of operations on the generalized
type.  [If identity implemented the generic routines, then specializations that fell back
to here would not have their specialized forms called in the generic routines (rather would
have the base forms called).]

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
      &optional
      init-value
      ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜fail (λ()(error 'bad-init-value)))
        )
      ➜
      (destructuring-bind
        (&key base) init-value
        (cond
          (base
            (setf (base tm) base)
            [➜ok tm]
            )
          (t [➜fail])
          ))))

  (defun-typed entangle ((tm-orig identity-tr) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        ;; (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜  
      (let(
            (tm-entangled (make-instance (type-of tm-orig)))
            )
        (setf (base tm-entangled) (entangle (base tm-orig)))
        [➜ok tm-entangled]
      )))

;;--------------------------------------------------------------------------------
;;tm-decl-only
;;
  (defmacro def-identity-tr-1 (f &rest args)
    `(defun-typed ,f ((tm identity-tr) ,@args &optional ➜)
       (,f (base tm) ,@args ➜)
       ))

  (def-identity-tr-1 r)
  (def-identity-tr-1 esr)
  (def-identity-tr-1 w instance)
  (def-identity-tr-1 esw instance)

  (def-identity-tr-1 ec◧r)
  (def-identity-tr-1 ec◧w instance)

  (def-identity-tr-1 c◧)
  (def-identity-tr-1 s)
  (def-identity-tr-1 a instance)
  (def-identity-tr-1 on-leftmost)
  (def-identity-tr-1 on-rightmost)

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (def-identity-tr-1 a◧ instance)
  (defun-typed d
    (
      (tm identity-tr)
      &optional spill ➜
      )
    (d (base tm) spill ➜)
    )
  (defun-typed d◧
    (
      (tm identity-tr)
      &optional spill ➜
      )
    (d◧ (base tm) spill ➜)
    )


;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
  (defun-typed heads-on-same-cell
    (
      (tm0 identity-tr)
      (tm1 identity-tr)
      &optional ➜
      )
    (heads-on-same-cell (base tm0) (base tm1) ➜)
    )

  (defun-typed heads-on-same-cell
    (
      (tm0 identity-tr)
      (tm1 tape-machine)
      &optional ➜
      )
    (heads-on-same-cell (base tm0) tm1 ➜)
    )

  (defun-typed heads-on-same-cell
    (
      (tm0 tape-machine)
      (tm1 identity-tr)
      &optional ➜
      )
    (heads-on-same-cell tm0 (base tm1) ➜)
    )



 
