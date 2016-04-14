#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Interval Space

  tm-interval defines a range of continguous cells from another tape machine's tape. 
  Because it is based on another tm (or in this case two tms) it is properly a transform.

  Read, write, allocate, etc, just pass through to the base machine.  However, our
  leftmost and rightmost may be different than the leftmost and rightmost of the base
  machines.

  An interval space is not to be confused with a subspace. A subspace occurs when
  a cell holds a tape.  See tm-subspace.lisp.

  Upon init, 
   If the :mount parameter is specified, and it is a tape machine, then the entire
   tape machine is the interval.  If it is a list, then the 

the :base option takes one or two tape machines.  If one machine is supplied,
  then the interval is understood to span the entire tape.  and it is not empty, then the
  interval has one cell, which is both the leftmost and the rightmost of the interval.

|#


(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-interval (tape-machine)())

  (defstruct interval
    leftmost ; a tape machine with head on the interval leftmost
    rightmost ; a tape machine *on the same tape* with head on the interval rightmost
    )

  (defmethod init 
    (
      (tm tm-interval)
      init-list 
      &optional
      (cont-ok (be t))
      (cont-fail (λ()(error 'bad-init-value)))
      )
    (destructuring-bind
      (&key base mount &allow-other-keys) init-list
      (let( tm-leftmost tm-rightmost )
        (cond
          ((∧ (consp base) (= (length base) 2))
            (setq tm-leftmost (dup (first base)))
            (setq tm-rightmost (dup (second base)))
            )
          ((∧ (consp base) (= (length base) 1))
            (setq tm-leftmost (dup (first base)))
            (setq tm-rightmost (dup (first base)))
            )
          ((∧ (¬ (consp base)) (typep base 'tape-machine ))
            (setq tm-leftmost (dup base))
            (setq tm-rightmost (dup base))
            )
          (t (funcall cont-fail)
            ))
        (cond
          ((consp mount)
            (as* tm-rightmost mount #'do-nothing cont-fail)
            )
          (mount
            (as tm-rightmost mount #'do-nothing cont-fail)
            ))
        (cond
          ((heads-on-same-cell tm-leftmost tm-rightmost)
            (change-class tm 'tm-singular)
            (init :tm-type {'tm-interval :base tm-leftmost} :mount (r tm-leftmost))
            )
          (t
            (setf (HA tm) (dup tm-leftmost))
            (setf (tape tm) (make-interval :leftmost tm-leftmost :rightmost tm-rightmost))
            (funcall cont-ok)
            )))))

;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod r ((tm tm-interval))
    (r (HA tm))
    )

  (defmethod w ((tm tm-interval) object)
    (w (HA tm) object)
    t
    )
 
  (defmethod cue-leftmost  ((tm tm-interval)) 
    (cue-to (HA tm) (interval-leftmost (tape tm)))
    t
    )

  (defmethod cue-rightmost  ((tm tm-interval)) 
    (cue-to (HA tm) (interval-rightmost (tape tm)))
    t
    )

  ;; intervals may be nested
  (defun heads-on-same-cell-interval (tm0 tm1 cont-true cont-false)
    (let(
          (base-0 tm0)
          (base-1 tm1)
          )
      (loop
        (unless (typep base-0 'tm-interval) (return))
        (setf base-0 (HA base-0))
        )
      (loop
        (unless (typep base-1 'tm-interval) (return))
        (setf base-1 (HA base-1))
        )
      (heads-on-same-cell base-0 base-1 cont-true cont-false)
      ))


  (defmethod heads-on-same-cell 
    (
      (tm0 tm-interval) 
      (tm1 tape-machine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-interval tm0 tm1 cont-true cont-false)
    )

  (defmethod heads-on-same-cell 
    (
      (tm0 tape-machine) 
      (tm1 tm-interval) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-interval tm0 tm1 cont-true cont-false)
    )

  (defmethod s
    (
      (tm tm-interval)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (if
      (heads-on-same-cell (HA tm) (interval-rightmost (tape tm)))
      (funcall cont-rightmost)
      (s (HA tm) cont-ok #'cant-happen) ; we just filtered out the rightmost case
      ))

  ;; allocate a cell
  ;;
  ;; All allocations within the interval space are part of the interval space, thus
  ;;  when allocation is made from rightmost, the rightmost bound is pushed out to 
  ;;  be on the newly allocated cell, etc.
  ;;
  ;; Note, tm-interval uses multiple heads on the same tape (that of HA, intervale-lefmost
  ;; and interval-rightmost, so if array, say, were to emulate allocation by moving data,
  ;; interval would have incorrect behavior, which is one of the reasons we don't do such
  ;; emulation.
  ;;
    (defmethod a
      (
        (tm tm-interval)
        object
        &optional
        (cont-ok (be t))
        (cont-no-alloc (λ()(error 'alloc-fail)))
        )
      (if
        (heads-on-same-cell (HA tm) (interval-rightmost (tape tm)))
        (progn
          (a (HA tm) cont-ok cont-no-alloc)
          (s (interval-rightmost (tape tm)))
          )
        (a (HA tm) cont-ok cont-no-alloc)
        ))

  (defmethod d 
    (
      (tm tm-interval)
      &optional 
      spill
      (cont-ok (be t))
      (cont-rightmost (λ()(error 'deallocation-request-at-rightmost)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (if
      (heads-on-same-cell (HA tm) (interval-rightmost (tape tm)))
      (funcall cont-rightmost)
      (d (HA tm) spill cont-ok cont-rightmost cont-no-alloc)
      ))


  ;; deallocate the leftmost cell
  (defmethod d◧
    (
      (tm tm-interval)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-no-dealloc (λ()(error 'dealloc-fail)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (let(
          (tm-leftmost (interval-leftmost (tape tm)))
          (tm-rightmost (interval-rightmost (tape tm)))
          (dealloc-object (r tm-leftmost))
          )

      (if (heads-on-same-cell tm-leftmost tm-rightmost)

        ;; collapse to empty
        (progn
          (when spill
            (as spill dealloc-object 
              #'do-nothing 
              (λ()(return-from d◧ (funcall cont-no-alloc)))
              ))
          (change-class tm 'tm-empty)
          (init tm {:tm-type {'tm-interval :base tm-leftmost}}
            (λ() (funcall cont-ok dealloc-object))
            #'cant-happen
            ))

        ;;normal dealloc, not singleton, so has at least two cells
        ;; swap objects, delete second cell
        (progn
          (w tm-leftmost (r-index tm-leftmost 1 #'do-nothing #'cant-happen))
          (d tm-leftmost spill cont-ok cont-no-dealloc cont-no-alloc)
          )
        )))
