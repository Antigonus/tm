#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;  helpers
;;
  ;; rightmost-index defined in tm-array-adj

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defun-typed r ((tm tm-array)) (aref (tape tm) (head tm) ))
  (defun-typed w ((tm tm-array) instance)
    (setf (aref (tape tm) (head tm)) instance)
    t
    )

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defun-typed c◧  ((tm tm-array)) 
    (setf (head tm) 0)
    )

;;--------------------------------------------------------------------------------
;;  head location predicates
;;
  (defun-typed heads-on-same-cell 
    (
      (tm0 tm-array) 
      (tm1 tm-array) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (if
      (= (head tm0) (head tm1))
      (funcall cont-true)
      (funcall cont-false)
      ))


;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defun-typed s
    (
      (tm tm-array)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (if
       (< (head tm) (rightmost-index tm))
       (progn
         (incf (head tm))
         (funcall cont-ok)
         )
      (funcall cont-rightmost)
      ))


;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defun-typed a 
    (
      (tm tm-array)
      instance 
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'tm-alloc-fail :text "alloc called in the middle of an array")))
      )
    (declare (ignore tm instance cont-ok))
    (funcall cont-no-alloc)
    )

;;--------------------------------------------------------------------------------
;; deallocating cells
;;
  (defun-typed d 
    (
      (tm tm-array)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-no-dealloc (λ()(error 'tm-deallocation-request-at-rightmost)))
      (cont-no-alloc (λ()(error 'tm-alloc-fail :text "can not spill")))
      )
    (declare (ignore tm spill cont-ok cont-no-alloc))
    (funcall cont-no-dealloc)
    )

    
