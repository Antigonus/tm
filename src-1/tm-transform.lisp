#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  This machine takes as initial values a tape machine and two functions, a forward
  transform and a reverse transform.

  When #'r is called, the base tape and the HA slot value are given to the read function,
  and the return value is returned as the machine value.

  When #'w is called, the base tape, the HA slot value, and the object that was passed
  to write are given to the write function.

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
    tm
    read
    write
    )

  (defmethod tm-init ((tm tm-transform) init-list)
    (destructuring-bind
      (
        base-tm
        &optional 
        initial-state
        (read (λ(tm state)(declare (ignore state))(r tm))) ; identity function
        (write (λ(tm state object)(declare (ignore state))(w tm object)))
        ) 
      init-list
      (setf (tape tm) 
        (make-transform :tm (dup base-tm) :read read :write write)
        )
      (setf (HA tm) initial-state)
      tm
      ))
          
;;--------------------------------------------------------------------------------
;; primitive methods
;;
  ;; we pass the base tape to the user's read function
  (defmethod r ((tm tm-transform)) 
    (funcall
      (transform-read (tape tm))
      (transform-tm (tape tm))
      (HA tm)
      ))

  ;; we pass the base tape and the object to be writen to the user's write function
  (defmethod w ((tm tm-transform) object)
    (funcall 
      (transform-write (tape tm))
      (transform-tm (tape tm)) 
      (HA tm) 
      object
      ))
 
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
