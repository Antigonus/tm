#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; tm-decl-only
;;
  (defun-typed c◧ ((tm ts1-parked) &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm ➜)
      ))

;;--------------------------------------------------------------------------------
;; tm-generic
;;
  (defun-typed c◨ ((tm ts1-parked) &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm ➜)
      ))

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;

;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;

;;--------------------------------------------------------------------------------
;; nd-tm-generic
;;
  (defun-typed s≠ 
    (
      (tm0 ts1-parked)
      (tm1 ts1-parked)
      &optional ➜
      )
    (bt:with-recursive-lock-held ((deed tm0))
      (call-next-method tm0 tm1 ➜)
      ))



                    
