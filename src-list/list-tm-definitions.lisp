#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defmethod r ((tm list-tm) &rest ⋯)
    (declare (ignore ⋯))
    (car (head tm))
    )

  (defmethod esr
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
      (funcall cont-ok (cadr (head tm)))
      (funcall cont-rightmost)
      ))

  (defmethod w ((tm list-tm) instance &rest ⋯)
    (declare (ignore ⋯))
    (setf (car (head tm)) instance)
    t
    )

  (defmethod esw
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
        (funcall cont-ok)
        )
      (funcall cont-rightmost)
      ))

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defmethod cue-leftmost ((tm list-tm) &rest ⋯)
    (declare (ignore ⋯)) 
    (setf (head tm) (tape tm))
    t
    )

;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defmethod s
    (
      (tm list-tm)
      &optional 
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (if 
      (cdr (head tm))
      (progn
        (setf (head tm) (cdr (head tm)))
        (funcall cont-ok)
        )
      (funcall cont-rightmost)
      ))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defmethod a
    (
      (tm list-tm)
      instance
      &optional
      (cont-ok (be t))
      (cont-no-alloc #'alloc-fail)
      )
    (declare (ignore cont-no-alloc))
    (let(
          (next-cell (cdr (head tm)))
          )
      (rplacd (head tm) (cons instance next-cell))
      (funcall cont-ok)
      ))

;;--------------------------------------------------------------------------------
;; location
;;  
  (defmethod on-leftmost 
    (
      tm
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (if
      (eq (head tm) (tape tm))
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defmethod on-rightmost
    (
      tm
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (if
      (¬ (cdr (head tm)))
      (funcall cont-true)
      (funcall cont-false)
      ))
