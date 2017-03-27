#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  These functions are shared by the parked and active states.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (defun-typed a◧ ((tm ts1-parked-active) instance &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm instance ➜)
      ))

  (defun-typed d◧ ((tm ts1-parked-active) &optional spill ➜)
    (prins (print "d◧ ts1-parked-active"))
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm spill ➜)
      ))
