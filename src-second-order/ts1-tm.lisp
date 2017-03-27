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
