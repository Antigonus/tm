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
;; accessing data
;;

  ;; disentangle uses tm-list's r-index, so I've defined this specific version so as not
  ;; to create any entangled tms in the function.
  (defun r-index-tm-list
    (
      tm
      index
      &optional 
      (cont-ok #'echo) 
      (cont-index-beyond-rightmost
        (λ() (error 'tm-read-beyond-rightmost :text "attempt to read beyond the rightmost allocated cell of the tape") ∅)
        )
      )
    (let(
          (node (HA tm)) ; this will be non-nil because tm is not void
          )
      (dotimes (i index)
        (if 
          (cdr node)
          (setq node (cdr node))
          (return-from r-index-tm-list (funcall cont-index-beyond-rightmost))
          ))
      (funcall cont-ok (car node))
      ))

  (defmethod r-index
    (
      (tm tm-list)
      index
      &optional 
      (cont-ok #'echo) 
      (cont-index-beyond-rightmost
        (λ() (error 'tm-read-beyond-rightmost :text "attempt to read beyond the rightmost allocated cell of the tape") ∅)
        )
      )
    (r-index-tm-list tm index cont-ok cont-index-beyond-rightmost)
    )

  (defun r◧-tm-list (tm) (car (tape tm)))

  (defmethod r◧
    (
      (tm tm-list)
      &optional
      (cont-ok #'echo) 
      (cont-void (λ()(error 'void-access)))
      )
    (declare (ignore cont-void))
    (funcall cont-ok (r◧-tm-list tm))
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
      (eq (tape tm0) (HA tm0))
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




        
