#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defun-typed r ((tm list-tm) &rest ⋯)
    (declare (ignore ⋯))
    (car (head tm))
    )

  (defun-typed esr
    (
      (tm list-tm)
      &optional 
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'step-from-rightmost)))
      &rest ⋯
      )
    (declare (ignore ⋯))
    (if
      (cdr (head tm))
      [cont-ok (cadr (head tm))]
      [cont-rightmost]
      ))

  (defun-typed w ((tm list-tm) instance &rest ⋯)
    (declare (ignore ⋯))
    (setf (car (head tm)) instance)
    t
    )

  (defun-typed esw
    (
      (tm list-tm)
      instance
      &optional 
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      &rest ⋯
      )
    (declare (ignore ⋯))
    (if
      (cdr (head tm))
      (progn
        (setf (cadr (head tm)) instance)
        [cont-ok]
        )
      [cont-rightmost]
      ))

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defun-typed cue-leftmost ((tm list-tm) &rest ⋯)
    (declare (ignore ⋯)) 
    (setf (head tm) (tape tm))
    t
    )

;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defun-typed s
    (
      (tm list-tm)
      &optional 
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      &rest ⋯
      )
    (declare (ignore ⋯))
    (if 
      (cdr (head tm))
      (progn
        (setf (head tm) (cdr (head tm)))
        [cont-ok]
        )
      [cont-rightmost]
      ))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defun-typed a
    (
      (tm list-tm)
      instance
      &optional
      (cont-ok (be t))
      (cont-no-alloc #'alloc-fail)
      &rest ⋯
      )
    (declare (ignore cont-no-alloc ⋯))
    (let(
          (next-cell (cdr (head tm)))
          )
      (rplacd (head tm) (cons instance next-cell))
      [cont-ok]
      ))

;;--------------------------------------------------------------------------------
;; location
;;  
  (defun-typed on-leftmost 
    (
      (tm list-tm)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      &rest ⋯
      )
    (declare (ignore ⋯))
    (if
      (eq (head tm) (tape tm))
      [cont-true]
      [cont-false]
      ))

  (defun-typed on-rightmost
    (
      (tm list-tm)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      &rest ⋯
      )
    (declare (ignore ⋯))
    (if
      (¬ (cdr (head tm)))
      [cont-true]
      [cont-false]
      ))
