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
      (cont-mount-failed (λ()(error 'tm-mount-failed)))
      )
    (cond
      ((eq (HA tm) 'parked)  
        (if (tape tm)
          (setf (HA tm) (tape tm))
          (funcall cont-mount-failed)
          ))
      ((cdr (HA tm))
        (setf (HA tm) (cdr (HA tm)))
        (funcall cont-ok)
        )
      (t
        (funcall cont-rightmost)
        )))


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
      (has-tape tm)
      (if 
        (is-parked tm)
        (tm-list-a◧&hp&t tm object cont-ok cont-no-alloc)
        (tm-list-a&h¬p tm object cont-ok cont-no-alloc)
        )
      (progn
        (tm-list-init-w-first-object tm object)
        (funcall cont-ok)
        )))

  (defun tm-list-a◧&hp&t (tm object cont-ok cont-no-alloc)
    (declare (ignore cont-no-alloc)) ;; should do something with this ..
    (setf (tape tm) (cons object (cdr (tape tm))))
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

  (defun tm-list-init-w-first-object (tm object)  
    (let(
          (new-cell (cons object ∅))
          )
      (setf (HA tm) new-cell)
      (setf (tape tm) new-cell)
      ))
          

;;--------------------------------------------------------------------------------
;; gather
;;
  ;; prepends cell to spill, steps spill making the end of spill the new attachment point
  (defun gs-list (spill cell)
    (rplacd cell (cdr (HA spill)))
    (rplacd (HA spill) cell)
    (setf (HA spill) cell)
    )

  ;; prepends cell to spill, no step
  (defun g-list (spill cell)
    (rplacd cell (cdr (HA spill)))
    (rplacd (HA spill) cell)
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
    (declare (ignore cont-no-alloc))
    (let(
          (cell-1 (cdr (HA tm))) ; cell-1 is the cell to be deallocated
          )
      (if
        cell-1
        (progn
          (rplacd (HA tm) (cdr cell-1)) ; re-route around cell-1
          (when
            spill
            (gs-list spill cell-1)
            )
          (funcall cont-ok (car cell-1))
          )
        ;;else there is no cell-1 to cut, no cell-2 to route to
        (funcall cont-rightmost)
        )))


        
