#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  From point of initiation every cell on the tape holds an integer.  The
  tape is read only.

  Leftmost holds an integer we call the 'infimum'.

  Given any two neighboring cells, the integer in the right hand neighbor, minus the
  integer in the left hand neighbor, yeilds a number we call the 'first-difference',
  represented by ∆.  For tm-line, the first difference is a constant.

  The rightmost cell holds an integer we call the bound.  If the supemum
  is set to ∅, then the there is no rightmost cell.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-line (tape-machine)())

  (defstruct line
    (infimum 0) ; the value for the leftmost cell, our integration constant
    (bound ∅) ; either ∅, or a boundary that we may not pass
    (∆ 1) ; a step increment
    )

  (defun tm-mk-line 
    (
      &optional
      init
      (cont-ok #'echo) 
      (cont-fail (λ() (error 'tm-mk-bad-init-type :text "expected a line struct")))
      )
    (let(
          (tm (make-instance 'tm-line))
          )
      (unless
        init
        (setf init (make-line))
        )
      (unless
        (typep init 'line)
        (funcall cont-fail)
        )
      (setf (HA tm) (line-infimum init))
      (setf (tape tm) init)
      (funcall cont-ok tm)
      ))

  (tm-mk-hook 'tm-line #'tm-mk-line)

;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod r ((tm tm-line)) (HA tm))

  (defmethod w ((tm tm-line) object)
    (error 'tm-read-only)
    t
    )
 
  (defmethod cue-leftmost  ((tm tm-line)) 
    (setf (HA tm) (car (tape tm)))
    t
    )

  ;; Interesting problem because the cons cell for tape
  ;; will copy and not be eq, for two duped machines.
  ;; The line is read only, so perhaps just a head number match
  ;; is good enough to say they are on the same cell.
  ;; We will go with that.
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
      (tm tm-line)
      object
      &optional
      cont-ok
      cont-no-alloc
      )
    (declare (ignore tm object cont-ok cont-no-alloc))
    (error 'tm-read-only)
    )

  (defmethod d 
    (
      (tm tm-line)
      &optional 
      spill
      cont-ok
      (cont-rightmost (λ()(error 'tm-deallocation-request-at-rightmost)))
      cont-no-alloc
      )
    (declare (ignore tm spill cont-ok cont-rightmost cont-no-alloc))
    (error 'tm-read-only)
    )