#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Region of Space

    A region is a set of contiguous cells from a tape. These contiguous cells occur on a
    'base machine'. tm-region defines a tape machine with a region as its tape.  Because
    the region machine is based on another tm, it is properly a transform.

    Read, write, allocate, etc, on the region machine just pass through to the base
    machine.  However, the region machine's leftmost and rightmost may be different than
    the leftmost and rightmost for the base machine.

    A region of space should not to be conflated with a subspace. In contrast to a region,
    a subspace occurs when a cell holds a tape machine as an object.  See
    tm-subspace.lisp.

  Address of a Region

    A region is located by yet another machine on the base tape. This machine is known as
    the 'location machine'.  A region lies to the right of the cell the location tape
    machine's head is on.  If the location machine is in the parked state, then the region
    begins with the leftmost cell on the base machine tape.

    The shortest length possible for a region is zero.  A zero length region machine is in
    the void state.  It is because of void regions that we adopted the 'lies to the right
    of the head of the location machine' convention.  We might have located a region
    with its leftmost cell, except that a void region doesn't have a leftmost cell.

    A singleton region has a single cell.  One may notice now that we have already been
    using singleton regions.  #'a may be thought of as a machine that creates a singleton
    region at the current head location,  while #'d may be thought of as a function that
    deletes a singleton region located at the current head location.

  Initializaiton

    A tm-region is intialized with one to three parameters:

    :location is required.  It locates the region within the base machine tape.  The
    region occurs to the right of the cell the base machine's head is on.

    :mount, if provided, is a list of objects to be allocated to the region using #'a*

    :rightmost, if provided, must be on the same tape as the base tape machine.  Its head
    address locates the rightmost cell of the region, this rightmost point must be to the
    right of the cell specified by :location.  Currently we do not verifiy this. Unlike
    the location machine, the rightmost machine, though on the base tape, has its head on
    a cell that lies within the region.  This has implication.  One example being that the
    region may not be directly deallocated due to entanglements with the rightmost
    machine.

  Deleting the location cell

    Suppose an operator on the base machine wishes to delete the location cell. The
    location cell is not in the region, so there can be no collision with machines in the
    region.  To do this, the operator must first adjust the location of the region to a
    cell that is not to be deallocated.  When doing so, it can not cause a machine in the
    region to leave the region.

  Void Regions and Neighbor Relations

    A region that is void still has a location.  

    If two void regions are located on the same cell, those two void regions have
    the same location.

    Suppose we have two adjacent regions, thus we have a left neighbor region and a right
    neighbor region. Notice that the location of the right neighbor region is the
    rightmost cell of the left neighbor region.

    Now suppose we want to make the left neighbor region void.  There is a problem,
    because the rightmost cell of the left neighbor region, which is acting as the
    location of the right neighbor region, is entangled.  We can not deallocte this
    rightmost cell.  

    Now as mentioned above, we can move the location machine for the right neighbor region
    to the left one cell, and then delete the rightmost cell of the left neighbor region.
    However, after doing this we have an ambiguous sitiation.  Both the now void
    leftneighbor region and the right neighbor region will have the same location.  They
    are in fact no longer left and right neighbors.  We have lost the order information.
 
    I wonder if this problem is not fundamental to using integer addressing. If we
    supported fractional addressing, we could maintain the relative order among
    void regions by giving them fractional addresses wthout moving to the address
    of the next cell.

    On a related note, there is an analogous problem of putting multiple subspaces within
    a cell.  This is done by creating a space of subspaces, called a manifold machine, and
    then including in the cells of the manifold the objects which are subspaces.  We then
    either have to assume that every contained tape machine is a manifold, or we have to
    externally keep track of the fact if a machine found as an object is a manifold that
    in turn holds multiple subspaces, or if it is directly a subspace.

    Hmm, we could also use manifolds to give order to multiple subspaces given at the
    same location, but the addresses in the manifold are simply a second integer in a
    pair of addresses - i.e. this is a fractional addressing scheme, though again,
    we need a bit to know if the fraction is present.  That bit could be the type
    of machine - a manifold machine type means the contents are a manifold.

  Void Base 

    When the base machine is void, there is only one possible location, that of void.

  Entanglement

    The region keeps three machines that are entangled with the base machine.  Those are
    the location machine, the region machine, and the rightmost cell of the region
    machine.  The indicated cell of the location machine is not in the region.  The
    indicated cells for the region machine and the rightmost machine are in the region.

|#


(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-region (tape-machine)())

  (defstruct region
    location ; region lies to the right of the indicated cell
    rightmost ; indicates the rightmost cell of the region
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
      (&key location mount rightmost &allow-other-keys) init-list

      (unless location (return-from init (funcall cont-fail)))

      (cond
        (rightmost
          (when 
            mount 
            (fas* location (mount mount)
              #'do-nothing
              (λ()(return-from init cont-fail))
              ))
          (setf (parameters tm) 
            (make-region 
              :location location
              :rightmost rightmost
              ))
          (let(
                (leftmost-region (fork location))
                )
            (s leftmost-region ;; leftmost-region one to right of location
              (λ()
                (setf (state tm) active)
                (setf (HA tm) leftmost-region)
                (setf (tape tm) ∅)
                (setf (entanglements tm) (make-entanglements tm))
                (funcall cont-ok)
                )
              cont-fail ; rightmost was provided, so must be able to step location
              )))
      
        (t ; rightmost was not provided
          (if 
            mount 

            ;; mount data, so just mount and call init again
            (let(
                  (rightmost-region (fork location))
                  )
              (as* rightmost-region (mount mount)
                (λ()
                  (init tm {:location location :rightmost rightmost-region})
                  (funcall cont-ok)
                  )
                cont-fail
                ))

            ;; no mount data, so region is void
            (progn
              (setf (parameters tm) 
                (make-region 
                  :location location
                  :rightmost ∅
                  ))
              (setf (state tm) void)
              (setf (HA tm) ∅)
              (setf (tape tm) ∅)
              (setf (entanglements tm) (make-entanglements tm))
              (funcall cont-ok)
              )))
        )))

;;--------------------------------------------------------------------------------
;; properties
;;
  (defmethod supports-dealloc ;; default behavior
    (
      (tm tm-region)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (declare (ignore cont-false))
    (funcall cont-true)
    )
  (defmethod supports-alloc ;; default behavior
    (
      (tm tm-region)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (declare (ignore cont-false))
    (funcall cont-true)
    )


;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defmethod r-0 ((tm tm-region) (state active) cont-ok cont-parked)
    (r-0 (HA tm) active cont-ok cont-parked)
    )

  (defmethod w-0 ((tm tm-region) (state active) object cont-ok cont-parked)
    (w-0 (HA tm) active object cont-ok cont-parked)
    t
    )
 
;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defmethod cue-leftmost-0  ((tm tm-region) (state parked) cont-ok cont-void)
    (let(
          (leftmost-region (region-location (parameters tm)))
          )
      (s leftmost-region
        (λ() 
          (setf (HA tm) leftmost-region)
          )
        #'cant-happen
        )))

  (defmethod cue-leftmost-0  ((tm tm-region) (state active) cont-ok cont-void)
    (let(
          (leftmost-region (region-location (parameters tm)))
          )
      (s leftmost-region
        (λ() 
          (setf (HA tm) leftmost-region)
          )
        #'cant-happen
        )))

  ;; regions cue quickly to rightmost
  (defmethod cue-rightmost-0  ((tm tm-region) (state active) cont-ok cont-void) 
    (let(
          (rightmost-region (region-rightmost (parameters tm)))
          )
      (setf (HA tm) rightmost-region)
      t
      ))

;;--------------------------------------------------------------------------------
;; head location predicate
;;
  (defmethod heads-on-same-cell-0 
    (
      (tm0 tm-region) (state0 active)
      (tm1 tm-region) (state1 active)
      cont-true
      cont-false
      cont-parked
      )
    (heads-on-same-cell-0 (HA tm0) active (HA tm1) active cont-true cont-false cont-parked)
    )

;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defmethod s-0 ((tm tm-region) (state active) cont-ok cont-rightmost)
    (heads-on-same-cell (HA tm) (region-rightmost (parameters tm))
      (λ()(funcall cont-rightmost))
      (λ()
        (s (HA tm) cont-ok #'cant-happen) ; we just filtered out the rightmost case
        )))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defmethod a◧-0
    (
      (tm tm-region)
      state
      object 
      cont-ok
      cont-not-supported
      cont-no-alloc
      )
    (a (region-location (parameters tm)) object cont-ok cont-not-supported cont-no-alloc)
    )

  (defmethod a◨-0 ((tm tm-region) (state active) object cont-ok cont-not-supported cont-no-alloc)
    (declare (ignore cont-not-supported))
    (as (region-rightmost (parameters tm)) object cont-ok cont-no-alloc)
    )

  (defmethod a-0
    (
      (tm tm-region)
      (state active)
      object 
      cont-ok
      cont-not-supported
      cont-no-alloc
      )
    (a (HA tm) object cont-ok cont-not-supported cont-no-alloc)
    )

;;--------------------------------------------------------------------------------
;; cell deallocation
;;
  ;; when this is called, tm state will parked or active, and there will be no collisions
  (defmethod d◧-0
    (
      (tm tm-region)
      cont-ok
      cont-not-supported
      )
    (let(
          (loc (region-location (parameters tm)))
          )
      (if (is-parked loc)
        (d◧-0 loc cont-ok cont-not-supported)
        (d-0 loc cont-ok cont-not-supported)
        )))

  ;; state will be active when this is called
  (defmethod d-0
    (
      (tm tm-region)
      cont-ok
      cont-not-supported 
      )
    (d-0 (HA tm) cont-ok cont-not-supported)
    )

;;--------------------------------------------------------------------------------
;; copying
;;
;;  default behavior

