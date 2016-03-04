#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch

  tm-subspace is yet another window into another tape machine.

  In this case read, write, allocate, etc, just pass through to the
  base machine.  However, our leftmost and rightmost may be different
  than the leftmost and rightmost of the base machine.

  Interesting, a subspace on a line, is an interval.  Interesting
  metaprogrammign potential here.

|#

(in-package #:le)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-subspace (tape-machine)())

  (defstruct subspace
    leftmost ; a tape machine with head on the subspace leftmost
    rightmost ; a tape machine *on the same tape* with head on the new rightmost
    )

  (defun mk-tm-subspace 
    (
      &optional
      init
      (cont-ok #'echo) 
      (cont-fail error 'tm-mk-bad-init-type :text "expected a line struct")
      )
    (let(
          (tm (make-instance 'tm-subspace))
          )
      (unless
        (∧
          init
          (typep init 'subspace)
          )
        (funcall cont-fail)
        )

      (setf (HA tm) (dup (subspace-leftmost init)))
      (setf (tape tm) init)
      (funcall cont-ok tm)
      ))

  (mk-tm-hook 'tm-subspace #'mk-tm-subspace)

;;--------------------------------------------------------------------------------
;; essential methods
;;
  (defmethod r ((tm tm-subspace))
    (r (HA tm))
    )

  (defmethod w ((tm tm-subspace) object)
    (w (HA tm) object)
    t
    )
 
  ;; already on leftmost
  (defmethod cue-leftmost  ((tm tm-subspace)) 
    (setf (HA tm) (car (tape tm)))
    t
    )

  ;; Interesting problem because the cons cell for tape
  ;; will copy and not be eq, for two duped machines.
  ;; The line is read only, so perhaps just a head number match
  ;; is good enough to say they are on the same cell.
  ;; We will go with that.
  (defun tms-on-same-cell-subspace-0 (tm0 tm1 cont-true cont-false)
    (if
      (∧
        (typep tm0 'tm-subspace)
        (typep tm1 'tm-subspace)
        (eql (HA tm0) (HA tm1))
        )
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defmethod tms-on-same-cell 
    (
      (tm0 tm-subspace) 
      (tm1 tape-machine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (tms-on-same-cell-subspace-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod tms-on-same-cell 
    (
      (tm0 tape-machine) 
      (tm1 tm-subspace) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (tms-on-same-cell-subspace-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod s
    (
      (tm tm-subspace)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (let(
          (sup (line-bound (tape tm)))
          (∆   (line-∆ (tape tm)))
          )
      (if
        (∧
          sup
          (> 
            (+ (HA tm) ∆)
            sup
            )
          )
        (funcall cont-rightmost)
        (progn
          (setf (HA tm) (+ (HA tm) ∆))
          (funcall cont-ok)
          ))))

  ;; allocate a cell
  (defmethod a
    (
      (tm tm-subspace)
      object
      &optional
      cont-ok
      cont-no-alloc
      )
    (declare (ignore tm object cont-ok cont-no-alloc))
    (error 'tm-read-only)
    )

  (defmethod -a◧-s
    (
      (tm tm-subspace)
      object
      &optional
      cont-ok
      (cont-no-alloc (error 'tm-alloc-fail))
      )
    (declare (ignore tm object cont-ok cont-no-alloc))
    (error 'tm-read-only)
    )
  
  (defmethod d 
    (
      (tm tm-subspace)
      &optional 
      spill
      cont-ok
      (cont-rightmost (λ()(error 'tm-deallocation-request-at-rightmost)))
      cont-no-alloc
      )
    (declare (ignore tm spill cont-ok cont-rightmost cont-no-alloc))
    (error 'tm-read-only)
    )

  (defmethod ◧d 
    (
      (tm tm-subspace)
      &optional 
      spill
      cont-ok 
      (cont-rightmost (λ()(error 'tm-deallocation-request-at-rightmost)))
      cont-no-alloc
      )
    (declare (ignore tm spill cont-ok cont-rightmost cont-no-alloc))
    (error 'tm-read-only)
    )

  ;; current value moved off rightmost, new value fills in.
  ;; fill is one before cell to be read from
  (defmethod m 
    (
      (tm tm-subspace)
      fill
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅)) ; rightmost of fill
      )
    (declare (ignore tm fill cont-ok cont-rightmost))
    (error 'tm-read-only)
    )

    
