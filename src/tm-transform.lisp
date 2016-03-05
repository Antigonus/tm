#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  This machine takes as initial values a tape machine and two functions, a forward
  transform and a reverse transform.

  When #'r is called, the init tape is read, the returned object is given to the forward
  transform, and the result is returned as the object read.

  When #'w is called, the object provided is given to the reverse-transform, and the result
  is written to the init tape.

  Provided that implementations are well behaved and use read and write to read from fill,
  and to write to spill, the transforms work when spilling and filling also.

  The forward or reverse transforms may be replaced with #'echo, to turn off the
  transform, or with (λ()(error 'tm-read-only)) (λ()(error 'tm-write-only)) to make the
  machine read or write only.  A write only machine may be useful, because the init tm can
  still be read.

|#


(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-transform (tape-machine)())

  (defstruct transform
    forward
    reverse
    tm
    )

  (defun mk-tm-transform
    (
      &optional
      init
      (cont-ok #'echo) 
      (cont-fail (λ() (error 'tm-mk-bad-init-type :text "expected a transform struct")))
      )
    (let(
          (tm (make-instance 'tm-transform))
          )
      (unless
        (∧
          init
          (typep init 'transform)
          )
        (funcall cont-fail)
        )
      (setf (tape tm) init)
      (funcall cont-ok tm)
      ))

  (mk-tm-hook 'tm-transform #'mk-tm-transform)

;;--------------------------------------------------------------------------------
;; essential methods
;;
  (defmethod r ((tm tm-transform)) 
    (funcall (transform-forward (tape tm)) tm)
    )

  (defmethod w ((tm tm-transform) object)
    (funcall (transform-reverse (tape tm)) tm object)
    )
 
  (defmethod cue-leftmost  ((tm tm-transform)) 
    (cue-leftmost (transform-tm (tape tm)))
    )

  (defun heads-on-same-cell-transform-0 (tm0 tm1 cont-true cont-false)
    (heads-on-same-cell-transform-0 
      (transform-tm (tape tm0))
      (transform-tm (tape tm1))
      cont-true
      cont-false
      ))

  (defmethod heads-on-same-cell 
    (
      (tm0 tm-transform) 
      (tm1 tape-machine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-transform-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod heads-on-same-cell 
    (
      (tm0 tape-machine) 
      (tm1 tm-transform) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-transform-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod s
    (
      (tm tm-transform)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (s (transform-tm (tape tm)) cont-ok cont-rightmost)
    )

  (defmethod a
    (
      (tm tm-transform)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    (a (transform-tm (tape tm)) object cont-ok cont-no-alloc)
    )

  (defmethod d 
    (
      (tm tm-transform)
      &optional 
      spill
      (cont-ok (be t))
      (cont-rightmost (λ()(error 'tm-deallocation-request-at-rightmost)))
      (cont-no-alloc 
        (λ()(error 'tm-alloc-fail :text "could not spill")))
      )
    (d (transform-tm (tape tm)) spill cont-ok cont-rightmost cont-no-alloc)
    )

  (defmethod ◧d 
    (
      (tm tm-transform)
      &optional 
      spill
      (cont-ok (be t))
      (cont-rightmost (λ()(error 'tm-deallocation-request-at-rightmost)))
      (cont-no-alloc
        (λ()(error 'tm-alloc-fail :text "could not spill")))
      )
    (◧d (transform-tm (tape tm)) spill cont-ok cont-rightmost cont-no-alloc)
    )
