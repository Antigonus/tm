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
      (init-value cons)
      &optional ➜
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

  (defun-typed init 
    (
      (tm identity-tr)
      (init-value identity-tr)
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        )
      ➜
      (setf (base tm) (mk (type-of (base tm)) tm)) ; makes an entangled copy
      [➜ok tm]
      ))

;;--------------------------------------------------------------------------------
;;tm-decl-only
;;
  (defmacro def-identity-tr-1 (f &rest args)
    {'defun-typed f {(q tm identity-tr) (o args) '&optional '➜}
      {f (q base tm) (o args) '➜}
      }
    )

  (def-identity-tr-1 r)
  (def-identity-tr-1 esr)
  (def-identity-tr-1 w instance)
  (def-identity-tr-1 esw instance)

  (def-identity-tr-1 r◧)
  (def-identity-tr-1 w◧ instance)

  (def-identity-tr-1 cue-leftmost)
  (def-identity-tr-1 s)
  (def-identity-tr-1 a instance)
  (def-identity-tr-1 on-leftmost)
  (def-identity-tr-1 on-rightmost)

;;--------------------------------------------------------------------------------
;;tm-generic
;;
  (def-identity-tr-1 cue-rightmost)
  (def-identity-tr-1 as instance) 
  (def-identity-tr-1 a&h◨ instance)
  (def-identity-tr-1 as&h◨ instance)

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

;;--------------------------------------------------------------------------------
;; nd-tm-generic
;;
  (defun-typed s≠ 
    (
      (tm0 identity-tr)
      (tm1 identity-tr)
      &optional ➜
      )
    (s≠ (base tm0) (base tm1) ➜)
    )

  (def-identity-tr-1 a◨ instance)


;;--------------------------------------------------------------------------------
;; entanglement
;;
  (defun-typed with-mk-entangled
    (
      (tm0 identity-tr)
      continuation
      )
    (with-mk-entangled (base tm0)
      (λ(base-tm1)
        (let(
              (tm1 (mk (type-of tm0) {:base base-tm1}))
              )
          [continuation tm1]
          ))))
