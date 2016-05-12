#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  From point of initiation every cell on the tape holds an integer.  The
  tape is read only.

  Leftmost holds an integer we call the 'infimum'.

  Given any two neighboring cells, the integer in the right hand neighbor, minus the
  integer in the left hand neighbor, yeilds a number we call the 'first-difference',
  represented by Δ.  For tm-line, the first difference is a constant.

  The rightmost cell holds an integer we call the bound.  If the supemum
  is set to ∅, then the there is no rightmost cell.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-line (tape-machine)())

  (defstruct tm-line-parms
    min ; the value for the leftmost cell
    max ; when ∅ there is no maximum 
      Δ ; a step increment
    )

  (defmethod init 
    (
      (tm tm-line)
      init-list
      &optional 
      (cont-ok (be t))
      (cont-fail (λ()(error 'bad-init-value)))
      )
    (destructuring-bind
      (&key seed &allow-other-keys) init-list
      (if
        (∧
          seed
          (≤ (length seed) 3)
          )
        (destructuring-bind
          (&optional (min 0) (max ∅) (Δ 1)) seed
          (setf (HA tm) min)
          (setf (tape tm) ∅) ; some generators may create a tape as a cache
          (setf (parameters tm) (make-tm-line-parms :min min :max max :Δ Δ))
          (setf (entanglements tm) (make-entanglements tm))
          (funcall cont-ok)
          )
        (funcall cont-fail)
        )))

;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod r ((tm tm-line)) (HA tm))

  (defmethod w ((tm tm-line) object)
    (error 'tm-read-only)
    t
    )
 
  (defmethod cue-leftmost  ((tm tm-line)) 
    (setf (HA tm) (tm-line-parms-min (parameters tm)))
    t
    )

  ;; Interesting problem because the cons cell for tape will copy and not be eq, for two
  ;; duped machines.  The line is read only, so perhaps just a head number match is good
  ;; enough to say they are on the same cell.  We will go with that.
  (defun heads-on-same-cell-line-0 (tm0 tm1 cont-true cont-false)
    (if
      (∧
        (typep tm0 'tm-line)
        (typep tm1 'tm-line)
        (eql (HA tm0) (HA tm1))
        )
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defmethod heads-on-same-cell 
    (
      (tm0 tm-line) 
      (tm1 tape-machine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-line-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod heads-on-same-cell 
    (
      (tm0 tape-machine) 
      (tm1 tm-line) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-line-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod s
    (
      (tm tm-line)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (let(
          (max (tm-line-parms-max (parameters tm)))
          (Δ   (tm-line-parms-Δ (parameters tm)))
          )
      (if max
        (let(
              (threshold (- max Δ))
              )
          (if
            (> (HA tm) threshold)
            (funcall cont-rightmost)
            (progn
              (setf (HA tm) (+ (HA tm) Δ))
              (funcall cont-ok)
              )))
        (progn
          (setf (HA tm) (+ (HA tm) Δ))
          (funcall cont-ok)
          ))
      ))

  ;; allocate a cell
  (defmethod a◧
    (
      (tm tm-line)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (be ∅))
      )
    (declare (ignore tm object cont-ok cont-no-alloc))
    (error 'not-supported) ; this needs to be a continuation ...
    )

  ;; allocate a cell
  (defmethod a
    (
      (tm tm-line)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (be ∅))
      )
    (declare (ignore tm object cont-ok cont-no-alloc))
    (error 'not-supported)
    )

  ;; deallocates the cell just to the right of the head
  (defmethod d 
    (
      (tm tm-line)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
      (cont-not-supported (λ()(error 'not-supported)))
      (cont-collision (λ()(error 'dealloc-entangled)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (declare (ignore
               spill
               cont-ok 
               cont-rightmost
               cont-collision
               cont-no-alloc
               ))
    (funcall cont-not-supported)
    )

  ;; deallocates the leftmost cell
  (defmethod d◧
    (
      (tm tm-line)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
      (cont-not-supported (λ()(error 'not-supported)))
      (cont-collision (λ()(error 'dealloc-entangled)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (declare (ignore
               spill
               cont-ok 
               cont-rightmost
               cont-collision
               cont-no-alloc
               ))
    (funcall cont-not-supported)
    )

