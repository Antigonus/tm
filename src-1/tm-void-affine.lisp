#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  The tape machine version of the bit bucket ...

  A singular affine machine, but reading always returns ∅.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-void-affine (tm-singular-affine)())

;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod r ((tm tm-void-affine))
    (declare (ignore tm))
    ∅
    )
  (defmethod w ((tm tm-void-affine) object)
    (declare (ignore tm object))
    t
    )
