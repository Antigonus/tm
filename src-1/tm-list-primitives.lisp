#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Tape is implemented with a singly linked list.

  When a machine is first created it will be of type empty projective, Upon 
  the allocation of a new cell, it then becomes a tm-list.

  Deallocation, #'d◧  of the last cell will cause tm-list to collapse back
  into projective machine.

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
  ;; deallocates the cell just to the right of the head
  (defmethod d 
    (
      (tm tm-list)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
      (cont-not-supported (λ()(error 'dealloc-not-supported)))
      (cont-entangled (λ()(error 'dealloc-entangled)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (declare (ignore cont-not-supported))
    (tm-list-on-rightmost tm
      cont-rightmost
      (λ()
          ;; as we elimated the rightost case, dealloc-cell will exist
          (let*(
                 (dealloc-cell (cdr (HA tm)))
                 (dealloc-object (car dealloc-cell))
                 (connection-point (cdr dealloc-cell))
                 )
            (entangled tm dealloc-cell
              cont-entangled
              (λ()
                (when spill
                  (as spill dealloc-object 
                    #'do-nothing 
                    (λ()(return-from d (funcall cont-no-alloc)))
                    ))
                (rplacd (HA tm) connection-point)
                (funcall cont-ok dealloc-object)
                )
              )))
      ))
     
