#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

We don't have to worry about synchronizing destructive operations on an empty machine.

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; tm-decl-only
;;

;;--------------------------------------------------------------------------------
;; tm-generic
;;
  (defun-typed as ((tm ea-empty) instance &optional ➜)
    (bt:with-recursive-lock-held ((tm deed))
      (call-next-method tm instance ➜)
      ))

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (defun-typed a◧ ((tm ea-empty) instance &optional ➜)
    (bt:with-recursive-lock-held ((tm deed))
      (call-next-method tm instance ➜)
      ))

;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
  ;; heads-on-same-cell is a synonym for (entangled tm0 tm1)

