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
  (defun-typed a◧ ((tm ea-tm) instance &optional ➜)
    (bt:with-recursive-lock-held ((tm deed))
      (call-next-method tm instance ➜)
      ))

  (defun-typed d◧ ((tm ea-tm) &optional spill ➜)
    (bt:with-recursive-lock-held ((tm deed))
      (call-next-method tm spill ➜)
      ))

;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
  (defun-typed entangled
    (
      (tm0 status-tm)
      (tm1 status-tm)
      &optional ➜
      )
    (bt:with-recursive-lock-held ((tm deed))
      (call-next-method tm0 tm1 ➜)
      ))
