#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; status-tm specific
;;
  (def-function-class park (tm &optional ➜))


;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
  (defun-typed entangled
    (
      (tm0 status-tm)
      (tm1 status-tm)
      &optional ➜
      )
    (entangled (base tm0) (base tm1) ➜)
    )


