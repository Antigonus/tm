#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Region of Space

  tm-region defines a range of continguous cells from another tape machine's tape. 
  Because it is based on another tm it is properly a transform.

  Read, write, allocate, etc, just pass through to the base machine.  However, the
  region's leftmost and rightmost may be different than the leftmost and rightmost
  of the base machine.

  A region of space is not to be confused with a subspace. A subspace occurs when a cell
  holds a sequence or tape machine as an object.  See tm-subspace.lisp.

  A tm-region is intialized with one to three parameters:

  :base is required.  It locates the region within the base space.  The region
  occures to the right of the cell the base machine's head is on.

  :mount, if provided, is a list of objects to be allocated to the region using #'a*

  :rightmost, if provided, must be a dup of the base tape machine.  Its head address
  becomes the rightmost of the region, this rightmost point must be to the right of the
  cell specified by :base.  Currently we do not verifiy this.

 

|#


(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-region (tape-machine)())

  (defstruct region
    location ; region lies to the right of this cell
    rightmost ; a tape machine *on the same tape* with head on the region rightmost 
    )

  (defmethod init 
    (
      (tm tm-region)
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
              (make-region 
                :location loocation
                :rightmost (dup rightmost)
                ))
            (s base ; base becomes leftmost after being stepped
              (λ()
                (setf (HA tm) (dup base))
                (when mount (a* location (mount mount) cont-ok cont-fail))
                (funcall cont-ok)
                )
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
                  (make-region 
                    :location location
                    :rightmost (dup base)
                    ))
                (funcall cont-ok)
                )
              cont-fail
              )))

        (t
          (change-class tm 'tm-void)
          (init tm {:tm-type {'tm-region :base base}})
          )
        )))

;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod r ((tm tm-region))
    (r (HA tm))
    )

  (defmethod w ((tm tm-region) object)
    (w (HA tm) object)
    t
    )
 
  (defmethod cue-leftmost  ((tm tm-region)) 
    (let(
          (tm (dup (region-location (tape tm))))
          )
      (s tm
        (λ() (cue-to (HA tm) tm))
        (λ() (error 'imppossible-to-get-here)) ; the region is not void
        )))

  (defmethod cue-rightmost  ((tm tm-region)) 
    (cue-to (HA tm) (region-rightmost (tape tm)))
    t
    )

  ;; regions may be nested
  (defun heads-on-same-cell-region (tm0 tm1 cont-true cont-false)
    (let(
          (base-0 tm0)
          (base-1 tm1)
          )
      (loop
        (unless (typep base-0 'tm-region) (return))
        (setf base-0 (HA base-0))
        )
      (loop
        (unless (typep base-1 'tm-region) (return))
        (setf base-1 (HA base-1))
        )
      (heads-on-same-cell base-0 base-1 cont-true cont-false)
      ))


  (defmethod heads-on-same-cell 
    (
      (tm0 tm-region) 
      (tm1 tape-machine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-region tm0 tm1 cont-true cont-false)
    )

  (defmethod heads-on-same-cell 
    (
      (tm0 tape-machine) 
      (tm1 tm-region) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-region tm0 tm1 cont-true cont-false)
    )

  (defmethod s
    (
      (tm tm-region)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (heads-on-same-cell (HA tm) (region-rightmost (tape tm))
      (λ()(funcall cont-rightmost))
      (λ()
        (s (HA tm) cont-ok #'cant-happen) ; we just filtered out the rightmost case
        )))

  ;; allocate a cell
  ;;
  ;; All allocations within the region space are part of the region space, thus
  ;;  when allocation is made from rightmost, the rightmost bound is pushed out to 
  ;;  be on the newly allocated cell, etc.
  ;;
  ;; Note, tm-region uses multiple heads on the same tape (that of HA, regione-lefmost
  ;; and region-rightmost, so if array, say, were to emulate allocation by moving data,
  ;; region would have incorrect behavior, which is one of the reasons we don't do such
  ;; emulation.
  ;;
    (defmethod a
      (
        (tm tm-region)
        object
        &optional
        (cont-ok (be t))
        (cont-no-alloc (λ()(error 'alloc-fail)))
        )
      (heads-on-same-cell (HA tm) (region-rightmost (tape tm))
        (λ()
          (a (HA tm) 
            (λ()
              (s (region-rightmost (tape tm)) #'do-nothing #'cant-happen)
              (funcall cont-ok)
              )
            cont-no-alloc
            ))
        (λ()
          (a (HA tm) cont-ok cont-no-alloc)
          )))

  (defmethod d 
    (
      (tm tm-region)
      &optional 
      spill
      (cont-ok (be t))
      (cont-rightmost (λ()(error 'deallocation-request-at-rightmost)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (heads-on-same-cell (HA tm) (region-rightmost (tape tm))
      cont-rightmost
      (λ()
        (d (HA tm) spill cont-ok cont-rightmost cont-no-alloc)
        )))


  ;; deallocate the leftmost cell
  (defmethod d◧
    (
      (tm tm-region)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-no-dealloc (λ()(error 'dealloc-fail)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (let(
          (tm-leftmost (region-leftmost (tape tm)))
          (tm-rightmost (region-rightmost (tape tm)))
          (dealloc-object (r tm-leftmost))
          )

      (heads-on-same-cell tm-leftmost tm-rightmost

        ;; collapse to empty
        (λ()
          (when spill
            (as spill dealloc-object 
              #'do-nothing 
              (λ()(return-from d◧ (funcall cont-no-alloc)))
              ))
          (change-class tm 'tm-void)
          (init tm {:tm-type {'tm-region :base (region-location tm)}}
            (λ() (funcall cont-ok dealloc-object))
            #'cant-happen
            ))

        ;;normal dealloc, not singleton, so has at least two cells
        ;; swap objects, delete second cell
        (λ()
          (w tm-leftmost (r-index tm-leftmost 1 #'do-nothing #'cant-happen))
          (d tm-leftmost spill cont-ok cont-no-dealloc cont-no-alloc)
          )
       )))
