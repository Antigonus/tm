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

  Address of a Region

    The address of a region within a space is the address of the cell just to the left of
    the leftmost cell belonging to the region.  An address may be represented by
    indicating the cell with a given address using a tape machine.  If such a tape machine
    is void, then the region extends from the leftmost cell of the tape.

    The smallest region is a void region, which has zero cells. It is because of void
    regions that we have adopted the one to the left addressing convention.

    A singleton region has a single cell.  Note though, because it is a region the address
    of a single cell region is the address of the left neighbor cell, rather than the
    address of the single cell itself.

    Within a region there is no way to access the cell that has the address being used to
    locate the region in space.  Hence, operations that make use of the region address
    are operations that belong to the space, rather than to the region. 

    Note that our deallocation function, #'d addresses the cell to be deallocated as
    though it is a singleton region.  #'d* then operates on non-singular regions.  #'d is
    an operator that belongs to a space, and is used to operate on regions. 

  Initializaiton

    A tm-region is intialized with one to three parameters:

    :base is required.  It locates the region within the base space.  The region occurs to
    the right of the cell the base machine's head is on.  A-typical to initialization, the
    base is not dupped. This is so the location can be moved left should the location cell
    be deallocated.

    :mount, if provided, is a list of objects to be allocated to the region using #'a*

    :rightmost, if provided, must be a dup of the base tape machine.  Its head address
    becomes the rightmost of the region, this rightmost point must be to the right of the
    cell specified by :base.  Currently we do not verifiy this.

    Deleting the location cell

    A region is located in space via the base machine.  The region lies just to the 
    right of the cell the base machine's head is on. 

    Suppose an operator on the base machine wishes to delete the location cell.  To
    do this, it must first adjust the location of the region to a cell that is not to 
    be deallocated.  When doing so, it can not cause a machine in the region to 
    leave the region.

  Void Region

    A region that is void still has a location.  

    Suppose that two regions are neighbors.  Then the location of the right neighbor
    region is the rightmost cell of the left neighbor region.  Now suppose the left
    neighbor region becomes void, this would be illegal, as it would require deleting the
    cell that another machine's head is on (i.e deleting the right neighbor location
    marker).

  Void Base 

    When the base is void, there is only one possible location, that of void.

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

      (unless (∧ base (¬ (typep base 'tm-void)))
        (return-from init (funcall cont-fail)))

      (cond
        (rightmost
          (let(
                (location base)
                (leftmost (dup base))
                )
            (setf (tape tm) 
              (make-region 
                :location location
                :rightmost rightmost
                ))
            (when mount (a* location (mount mount) cont-ok cont-fail))
            (s leftmost ; base becomes leftmost after being stepped
              (λ()
                (setf (HA tm) leftmost)
                (setf (entanglements tm) (make-entanglements tm))
                (funcall cont-ok)
                )
              cont-fail ; rightmost was provided, so must be able to step base
              )))
      
        (mount
          (let(
                (location base)
                (leftmost (dup base))
                (tm-data (mount mount))
                )
            (as leftmost (r tm-data) ; after step, base is leftmost
              (λ()
                (setf (HA tm) leftmost)
                (let(
                      (i (dup leftmost)) ; general use iterator
                      )
                  (s tm-data (λ()(as* i  tm-data)) #'do-nothing) ; i is rightmost after this
                  (setf (tape tm) 
                    (make-region 
                      :location location
                      :rightmost i
                      ))
                  (setf (entanglements tm) (make-entanglements tm))
                  (funcall cont-ok)
                ))
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
          (a (HA tm) object 
            (λ()
              (s (region-rightmost (tape tm)) #'do-nothing #'cant-happen)
              (funcall cont-ok)
              )
            cont-no-alloc
            ))
        (λ()
          (a (HA tm) object cont-ok cont-no-alloc)
          )))

  (defmethod d
    (
      (tm tm-region)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
      (cont-not-supported (λ()(error 'dealloc-not-supported)))
      (cont-entangled (λ()(error 'dealloc-entangled)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (heads-on-same-cell (HA tm) (region-rightmost (tape tm))
      cont-rightmost
      (λ()
        (d (HA tm) spill cont-ok cont-rightmost cont-not-supported cont-entangled cont-no-alloc)
        )))


  ;; deallocate the leftmost cell
  ;; we wouldn't be here if the region were void, thus there must be one cell
  ;; and if the region isn't void, neither is the space.
  (defmethod d◧
    (
      (tm tm-region)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
      (cont-not-supported (λ()(error 'dealloc-not-supported)))
      (cont-entangled (λ()(error 'dealloc-entangled)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (let(
          (location (region-location (tape tm)))
          (leftmost (dup (region-location (tape tm)))) ; will be leftmost after being stepped
          (rightmost (region-rightmost (tape tm)))
          )
      (s leftmost #'do-nothing #'cant-happen)
      (let(
            (dealloc-object (r leftmost))
            )
        (heads-on-same-cell leftmost rightmost
          ;; collapse to void
          (λ()
            (when spill
              (as spill dealloc-object 
                #'do-nothing 
                (λ()(return-from d◧ (funcall cont-no-alloc)))
                ))
            (change-class tm 'tm-void)
            (init tm {:tm-type {'tm-region :base location}}
              (λ() (funcall cont-ok dealloc-object))
              #'cant-happen
              ))
          ;;normal dealloc
          (λ()
            (d location spill cont-ok cont-rightmost cont-not-supported cont-entangled cont-no-alloc)
            )))))
