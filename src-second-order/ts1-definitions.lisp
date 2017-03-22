#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; ts1-tm specific
;;

;;--------------------------------------------------------------------------------
;; tm-decl-only
;;

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
;; more specific versions can be found for status-abandoned and status-empty,
;; so these will only apply to status-parked and status-active
;;
  (defun-typed a◧ ((tm ts1-tm) instance &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm instance ➜)
      ))

  (defun-typed d◧ ((tm ts1-tm) &optional spill ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm spill ➜)
      ))

;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
  (defun-typed entangled
    (
      (tm0 ts1-tm)
      (tm1 ts1-tm)
      &optional ➜
      )
    (bt:with-recursive-lock-held ((deed tm0))
      (bt:with-recursive-lock-held ((deed tm1))
        (call-next-method tm0 tm1 ➜)
        )))
