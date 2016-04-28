#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Interval Space

  tm-interval defines a range of continguous cells from another tape machine's tape. 
  Because it is based on another tm it is properly a transform.

  Read, write, allocate, etc, just pass through to the base machine.  However, our
  leftmost and rightmost may be different than the leftmost and rightmost of the base
  machines.

  An interval space is not to be confused with a subspace. A subspace occurs when
  a cell holds a sequence or tape machine as an object.  See tm-subspace.lisp.

  :base  - specifies the location of the interval
  :mount - is a list of objects to put in the interval
  :rightmost - is a machine that marks the rightmost cell of the interval at the
       time of initialization.
  
  :base is required.

 

|#


(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-interval (tape-machine)())

  (defstruct interval
    location ; interval lies to the right of this cell
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
      (&key base mount rightmost &allow-other-keys) init-list

      (unless base  (return-from init (funcall cont-fail)))

      (cond
        (rightmost
          (let(
                (location (dup base))
                )
            (setf (tape tm) 
              (make-interval 
                :location loocation
                :rightmost (dup rightmost)
                ))
            (s base ; base becomes leftmost after being stepped
              (λ()
                (setf (HA tm) (dup base))
                (if mount
                  (let(
                        (tm-data (mount mount))
                        )
                    (a* location tm-data cont-ok cont-fail)
                    )
                  (funcall cont-ok)
                  ))
              cont-fail ; rightmost was provided, so must be able to step base
              )))
      
        (mount
          (let(
                (tm-data (mount mount))
                (location (dup base))
                )
            (as base (r tm-data) ; after step, base is leftmost
              (λ()
                (setf (HA tm) (dup base))
                (s tm-data (λ()(as* base tm-data)) #'do-nothing) ; base becomes rightmost
                (setf (tape tm) 
                  (make-interval 
                    :location location
                    :rightmost (dup base)
                    ))
                (funcall cont-ok)
                )
              cont-fail
              )))

        (t
          (change-class tm 'tm-void)
          (init tm {:tm-type {'tm-interval :base base}})
          )
        )))

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
    (let(
          (tm (dup (interval-location (tape tm))))
          )
      (s tm
        (λ() (cue-to (HA tm) tm))
        (λ() (error 'imppossible-to-get-here)) ; the interval is not empty
        )))

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
    (heads-on-same-cell (HA tm) (interval-rightmost (tape tm))
      (λ()(funcall cont-rightmost))
      (λ()
        (s (HA tm) cont-ok #'cant-happen) ; we just filtered out the rightmost case
        )))

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
      (heads-on-same-cell (HA tm) (interval-rightmost (tape tm))
        (λ()
          (a (HA tm) 
            (λ()(s (interval-rightmost (tape tm)) #'do-nothing #'cant-get-here))
            cont-no-alloc
            ))

          )
        (λ()
          (a (HA tm) cont-ok cont-no-alloc)
          )))

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
          (change-class tm 'tm-void)
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
