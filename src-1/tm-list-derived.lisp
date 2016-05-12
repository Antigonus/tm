#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  These routines specialize the tm-derived functions.  They are intended to be faster
  while producing the exact same results.  Hence, commenting out any one of these
  routines should not have any logically verifiable effect on the program.


|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; length
;;
  (defun tm-list-singleton (tm0)
    (¬ (cdr (tape tm0)))
    )

  (defmethod singleton 
    (
      (tm0 tm-list)
      &optional
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    (if 
      (tm-list-singleton tm0)
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defun tm-list-doubleton (tm0)
    (∧
      (cdr (tape tm0))
      (¬ (cddr (tape tm0)))
    ))

  (defmethod doubleton 
    (
      (tm0 tm-list)
      &optional
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    (if
      (tm-list-doubleton tm0)
      (funcall cont-true)
      (funcall cont-false)
      ))

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defmethod r◧
    (
      (tm tm-list)
      &optional
      (cont-ok #'echo) 
      (cont-void (λ()(error 'void-access)))
      )
    (declare (ignore cont-void))
    (car (tape tm))
    )

;;--------------------------------------------------------------------------------
;; absolute head placement
;;

;;--------------------------------------------------------------------------------
;;  head location predicates
;;

  (defun tm-list-on-rightmost (tm0 &optional (cont-true (be t)) (cont-false (be ∅)))
    (if
      (cdr (HA tm0))
      (funcall cont-false)
      (funcall cont-true)
      ))

  (defmethod on-rightmost
    (
      (tm0 tm-list)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (tm-list-on-rightmost tm0 cont-true cont-false)
    )

  (defun tm-list-on-leftmost (tm0  &optional (cont-true (be t)) (cont-false (be ∅)))
    (if
      (eq (cdr (tape tm0)) (cdr (HA tm0)))
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defmethod on-leftmost
    (
      (tm0 tm-list)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (tm-list-on-leftmost tm0 cont-true cont-false)
    )


;;--------------------------------------------------------------------------------
;; head stepping
;;


;;--------------------------------------------------------------------------------
;; deallocating cells
;;




        
