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
  (defun-typed -s* ((tm ts1-tm) &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm ➜)
      ))

  (defun-typed s ((tm ts1-tm) &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm ➜)
      ))

  (defun-typed -s ((tm ts1-tm) &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm ➜)
      ))

  (defun-typed a ((tm ts1-tm) instance &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      ;; (prins (print "a ts1-tm"))
      (call-next-method tm instance ➜)
      ))

  (defun-typed on-leftmost ((tm ts1-tm) &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm ➜)
      ))

  (defun-typed on-rightmost ((tm ts1-tm) &optional ➜)
    (bt:with-recursive-lock-held ((deed tm))
      (call-next-method tm ➜)
      ))

;;--------------------------------------------------------------------------------
;; tm-generic
;;
  ;;--------------------------------------------------------------------------------
  ;; cueing
  ;;  
    (defun-typed s* ((tm ts1-tm) &optional ➜)
      (bt:with-recursive-lock-held ((deed tm))
        (call-next-method tm ➜)
        ))

  ;;--------------------------------------------------------------------------------
  ;; cell allocation
  ;;
    (defun-typed as ((tm ts1-tm) instance &optional ➜)
      (bt:with-recursive-lock-held ((deed tm))
        ;; (prins (print "as ts1-tm"))
        (call-next-method tm instance ➜)
        ))

    (defun-typed a&s* 
      (
        (tm ts1-tm)
        instance
        &optional ➜
        )
      (bt:with-recursive-lock-held ((deed tm))
        (call-next-method tm instance ➜)
        ))

    (defun-typed as&s* 
      (
        (tm ts1-tm)
        instance
        &optional ➜
        )
      (bt:with-recursive-lock-held ((deed tm))
        (call-next-method tm instance ➜)
        ))


;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  ;;--------------------------------------------------------------------------------
  ;; cell allocation
  ;;
    (defun-typed epa ((tm ts1-tm) instance &optional ➜)
      (bt:with-recursive-lock-held ((deed tm))
        ;; (prins (print "epa ts1-tm"))
        (call-next-method tm instance ➜)
        ))

  ;;--------------------------------------------------------------------------------
  ;; cell deallocation
  ;;
    (defun-typed d ((tm ts1-tm) &optional spill ➜)
      (bt:with-recursive-lock-held ((deed tm))
        ;; (prins (print "d ts1-tm"))
        (call-next-method tm spill ➜)
        ))

    (defun-typed epd ((tm ts1-tm) &optional spill ➜)
      (bt:with-recursive-lock-held ((deed tm))
        ;; (prins (print "epd ts1-tm"))
        (call-next-method tm spill ➜)
        ))

;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
  (defun-typed heads-on-same-cell 
    (
      (tm0 ts1-tm)
      (tm1 ts1-tm)
      &optional ➜
      )
    (bt:with-recursive-lock-held ((deed tm0))
      (bt:with-recursive-lock-held ((deed tm1))
        (call-next-method tm0 tm1 ➜)
        )))

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

;;--------------------------------------------------------------------------------
;; nd-tm-generic
;;
  ;;--------------------------------------------------------------------------------
  ;; stepping with a boundary, boundaries are inclusive
  ;;
    (defun-typed s≠ 
      (
        (tm0 ts1-tm)
        (tm1 ts1-tm)
        &optional ➜
        )
      (bt:with-recursive-lock-held ((deed tm0))
        (bt:with-recursive-lock-held ((deed tm1))
          (call-next-method tm0 tm1 ➜)
          )))

  ;;--------------------------------------------------------------------------------
  ;; cell allocation
  ;;
    (defun-typed es*a
      (
        (tm ts1-tm)
        instance
        &optional ➜
        )
      (bt:with-recursive-lock-held ((deed tm))
        ;; (prins (print "es*a ts1-tm"))
        (call-next-method tm instance ➜)
        ))
      
