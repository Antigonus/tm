#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Tape is implemented with a singly linked list.

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
  ;; If the machine is empty, allocates a first cell,
  ;; otherwise allocates a cell just to the right of the head
  ;; The new cell initialized with the provided object.
  (defmethod a 
    (
      (tm tm-list)
      object 
      &optional 
      (cont-ok (be t))
      (cont-no-alloc (be ∅))
      )
    (if
      (tape tm)
      (if 
        (parked tm)
        (tm-list-a◧&hp&t tm object cont-ok cont-no-alloc)
        (tm-list-a&h¬p tm object cont-ok cont-no-alloc)
        )
      (progn
        (tm-list-a&hp&¬t tm object)
        (funcall cont-ok)
        )))

  (defun tm-list-a◧&hp&t (tm object cont-ok cont-no-alloc)
    (declare (ignore cont-no-alloc)) ;; should do something with this ..
    (setf (tape tm) (cons object (tape tm)))
    (funcall cont-ok)
    )

  (defun tm-list-a&h¬p (tm object cont-ok cont-no-alloc)
    (declare (ignore cont-no-alloc)) ;; should do something with this ..
    (let(
          (new-cell (cons object (cdr (HA tm))))
          )
      (rplacd (HA tm) new-cell)
      (funcall cont-ok)
      ))

  (defun tm-list-a&hp&¬t (tm object)  
    (setf (tape tm) (cons object ∅))
    )
          

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
      (cont-rightmost (λ()(error 'tm-deallocation-request-at-rightmost)))
      cont-no-alloc
      )
    (if
      (tape tm)
      (if
        (parked tm)
        (tm-list-d◧&hp&t tm spill cont-ok cont-rightmost cont-no-alloc)
        (tm-list-d&h¬p tm spill cont-ok cont-rightmost cont-no-alloc)
        )
      (funcall cont-rightmost)
      ))

   ;; tm-list-d◧ - delete leftmost
   ;; Contract says tape exists, so we can't get a cont-rightmost (which would signify
   ;; nothing to the right of the attachment point).
   ;; Contract says the head is parked, so we can't delete the cell the head is on.
   ;; Spill is a tm, but we don't know if it is a tm-list.
   ;; We allow that spill has been set to ∅ to turn off spilling
   (defun tm-list-d◧&hp&t (tm spill cont-ok cont-rightmost cont-no-alloc)
     (declare (ignore cont-rightmost)) ; as there is a tape, there is a leftmost to dealloc
     (let(
           (leftmost-object (car (tape tm)))
           )
       (setf (tape tm) (cdr (tape tm)))
       (if 
         spill 
         (as spill leftmost-object (λ()(funcall cont-ok leftmost-object)) cont-no-alloc)
         (funcall cont-ok leftmost-object)
         )))

   ;; This is d for the tape space.
   ;; Contract says the head is not parked, so this is our normal case, i.e. we
   ;; want to delete the cell to the right of the cell the head is on.
   (defun tm-list-d&h¬p (tm spill cont-ok cont-rightmost cont-no-alloc)
       (if
         (cdr (HA tm))
         (let*(
                (deallocation-cell (cdr (HA tm)))
                (affected-object (car deallocation-cell))
                (reconnect-point (cdr deallocation-cell))
                )
           (rplacd (HA tm) reconnect-point)
           (if spill
             (as spill affected-object (λ()(funcall cont-ok affected-object)) cont-no-alloc)
             (funcall cont-ok affected-object)
             ))
         (funcall cont-rightmost)
         ))
