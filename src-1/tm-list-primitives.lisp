#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Tape is implemented with a singly linked list.

  A machine is born as void projective,  with the addition of the first
  cell it becomes singular projective, and then, upon this branch of
  development it arrives at the tm-list.  And thus a tm-list will
  have at least two cells.

  Deallocation, #'d may cause this machine to collapse into a singular
  projective machine.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defmethod r ((tm tm-list)) (car (HA tm)))
  (defmethod w ((tm tm-list) object) (setf (car (HA tm)) object) t)

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  ;; our tape is never nil, so this returns true
  (defmethod cue-leftmost  ((tm tm-list)) 
    (setf (HA tm) (tape tm))
    tm
    )
  
;;--------------------------------------------------------------------------------
;;  head location predicates
;;
  (defmethod heads-on-same-cell 
    (
      (tm0 tm-list) 
      (tm1 tm-list) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (if
      ;; compares pointers, can't compare objects
      ;; our boundary value calculus causes this test to be complete (without end cases)
      (eq (cdr (HA tm0)) (cdr (HA tm1))) 
      (funcall cont-true)
      (funcall cont-false)
      ))


;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defmethod s
    (
      (tm tm-list)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (if 
      (cdr (HA tm))
      (progn
        (setf (HA tm) (cdr (HA tm)))
        (funcall cont-ok)
        )
      (funcall cont-rightmost)
      ))


;;--------------------------------------------------------------------------------
;; cell allocation
;;
  ;; Allocates a cell to the right of the head.
  (defmethod a 
    (
      (tm tm-list)
      object 
      &optional 
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (declare (ignore cont-no-alloc)) ;; should do something with this ..
    (let*(
           (connection-point (cdr (HA tm)))
           (new-cell (cons object connection-point))
           )
      (rplacd (HA tm) new-cell)
      (funcall cont-ok)
      ))
          

;;--------------------------------------------------------------------------------
;; deallocating cells
;;
  ;; a tm-list has at least two cells (or it would have collapsed to singular or void)
  ;; deallocates the cell just to the right of the head
  (defmethod d 
    (
      (tm tm-list)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-no-dealloc (λ()(error 'dealloc-fail)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )

    (tm-list-on-rightmost tm
      (λ() (funcall cont-no-dealloc) )
      (λ()
        (if (tm-list-doubleton tm) 

          ;; doubleton, then collapses to singular
          ;; as we eliminated the rightmost case, head is on leftmost
          (let*(
                 (keep-object (car (tape tm)))
                 (dealloc-cell (cdr (tape tm)))
                 (dealloc-object (car dealloc-cell))
                 )
            (when spill
              (as spill dealloc-object 
                #'do-nothing 
                (λ()(return-from d (funcall cont-no-alloc)))
                ))
            (change-class tm 'tm-singular)
            (init tm {:tm-type 'tm-list :mount keep-object})
            (funcall cont-ok dealloc-object)
            )
          
          ;; normal case, tape is longer than doubleton
          ;; as we elimated the rightost case, dealloc-cell will exist
          (let*(
                 (dealloc-cell (cdr (HA tm)))
                 (dealloc-object (car dealloc-cell))
                 (connection-point (cdr dealloc-cell))
                 )
            (when spill
              (as spill dealloc-object 
                #'do-nothing 
                (λ()(return-from d (funcall cont-no-alloc)))
                ))
            (rplacd (HA tm) connection-point)
            (funcall cont-ok dealloc-object)
            )))))
     

