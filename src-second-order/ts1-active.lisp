#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Currently these are supported status:

abandoned
parked
empty
active

There is no function on the tm interface that can be called to change the status
of an active machine.  'delete' of the last cell, for example, will result in 
a collision error.  Hence behavior is inherited from the identity transform.

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; ts1-tm definitions
;;
  ;; park is a synonym for c◧


;;--------------------------------------------------------------------------------
;; tm-decl-only
;;
  (defun-typed c◧ ((tm ts1-active) &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm ➜)
      ))

  (defun-typed s ((tm ts1-active) &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm ➜)
      ))

  (defun-typed -s ((tm ts1-active) &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm ➜)
      ))

  (defun-typed a ((tm ts1-tm) instance &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm instance ➜)
      ))

  (defun-typed on-leftmost ((tm ts1-active) &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm ➜)
      ))

  (defun-typed on-rightmost ((tm ts1-active) &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm ➜)
      ))

;;--------------------------------------------------------------------------------
;; tm-generic
;;
  (defun-typed c◨ ((tm ts1-active) &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm ➜)
      ))

  (defun-typed as ((tm ts1-active) instance &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm instance ➜)
      ))

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
        
;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
  (defun-typed heads-on-same-cell 
    (
      (tm0 ts1-active)
      (tm1 ts1-active)
      &optional ➜
      )
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm0 tm1 ➜)
      ))
