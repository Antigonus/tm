#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch

  The cell type for this machine is unique. It is a int-interval cell type.
  There are no generators for cells of this type.

  The tape on this machine is intialized with exactly one such cell, a single cell.  Hence
  it has a single cell of a unique type. 

  It is illegal to deallocate the cell under the head, and as this machine has one cell,
  and the head is always on top of it, no cell can ever be deallocated.  As it can not
  be deallocated it can not be shared or gathered.

|#

(in-package #:le)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-int-interval (tape-machine)())

  ;; init is an interval, #'car being the infinum, and #'cdr the suprenum
  ;; if the suprenum is ∅, the interval is open on the right
  (defun mk-tm-int-interval (&optional init (cont-ok #'echo) cont-fail)
    (declare (ignore cont-fail))
    (let(
          (tm (make-instance 'tm-int-interval))
          )
      (if
        init
        (progn
          (Unless
            (∧
              (consp init)
              (numberp (car init))
              (∨
                (¬ (cdr init))
                (numberp (cdr init))
                ))
            (error 'tm-mk-bad-init-type :text "expected a pair of numbers")
            )
          (setf (HA tm) (car init))
          (setf (tape tm) init)
          )
        (progn
          (setf (HA tm) 0)
          (setf (tape tm) (cons 0 ∅))
          ))
      (funcall cont-ok tm)
      ))


;;--------------------------------------------------------------------------------
;; essential methods
;;
  (defmethod r ((tm tm-int-interval)) (HA tm))

  (defmethod w ((tm tm-int-interval) object)
    (error 'tm-read-only)
    t
    )
 
  ;; already on leftmost
  (defmethod cue-leftmost  ((tm tm-int-interval)) 
    (declare (ignore tm))
    t
    )

  (defun tms-on-same-cell-0 (tm0 tm1 cont-true cont-false)
    (if
      (∧
        (typep tm0 'tm-int-interval)
        (typep tm1 'tm-int-interval)
        (eq (HA tm0) (HA tm1))
        )
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defmethod tms-on-same-cell 
    (
      (tm0 tm-int-interval) 
      (tm1 tape-machine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (tms-on-same-cell-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod tms-on-same-cell 
    (
      (tm0 tape-machine) 
      (tm1 tm-int-interval) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (tms-on-same-cell-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod s
    (
      (tm tm-int-interval)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (declare (ignore cont-ok))
    (funcall cont-rightmost)
    )

  ;; allocate a cell .. but can't
  (defmethod a
    (
      (tm tm-int-interval)
      object
      &optional
      cont-ok
      (cont-no-alloc (error 'tm-alloc-fail))
      )
    (declare (ignore tm object cont-ok))
    (funcall cont-no-alloc)
    )

  (defmethod -a◧-s
    (
      (tm tm-int-interval)
      object
      &optional
      cont-ok
      (cont-no-alloc (error 'tm-alloc-fail))
      )
    (declare (ignore tm object cont-ok))
    (funcall cont-no-alloc)
    )

  
  ;; there is no where to get a free cell of the correct type to 
  ;; call this routine with.
  ;; 
  (defmethod g
    (
      (tm tm-int-interval) 
      cell-reference
      &optional
      cont-ok 
      cont-no-alloc
      )
    (declare (ignore tm cell-reference cont-ok cont-no-alloc))
    (error 'tm-wrong-cell-type :text "cannot gather cells of this type")
    )

  (defmethod d 
    (
      (tm tm-int-interval)
      &optional 
      spill
      cont-ok
      (cont-rightmost (λ()(error 'tm-deallocation-request-at-rightmost)))
      cont-no-alloc
      )
    (declare (ignore tm spill cont-ok cont-no-alloc))
    (funcall cont-rightmost)
    )

  (defmethod ◧d 
    (
      (tm tm-int-interval)
      &optional 
      spill
      cont-ok 
      (cont-rightmost (λ()(error 'tm-deallocation-request-at-rightmost)))
      cont-no-alloc
      )
    (declare (ignore tm spill cont-ok cont-no-alloc))
    (funcall cont-rightmost)
    )

  ;; current value moved off rightmost, new value fills in.
  ;; fill is one before cell to be read from
  (defmethod m 
    (
      (tm tm-int-interval)
      fill
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅)) ; rightmost of fill
      )
    (w tm (r fill))
    (s fill
      cont-ok
      cont-rightmost
      ))

