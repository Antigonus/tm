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

    A singular region has a single cell.  One may notice now that we have already been
    using singular regions.  #'a may be thought of as a machine that creates a singular
    region at the current head location,  while #'d may be thought of as a function that
    deletes a singular region located at the current head location.

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
;; print
;;
  (defun print-indicated (tm0 tm)
    (heads-on-same-cell tm0 tm
      (λ()
        (princ "[")
        (princ (r tm0))
        (princ "]")
        )
      (λ()
        (princ (r tm0))
        )))

  (defmethod print-machine ((tm tm-region) &optional (n 0))
    (indent n) (princ tm) (nl)
    (indent n) (princ "state: ") (princ (type-of (state tm)))(nl)
    (indent n) (princ "location: ") (nl)
    (print-machine (region-location (parameters tm)) (1+ n))
    (indent n) (princ "rightmost: ") (nl)
    (print-machine (region-rightmost (parameters tm)) (1+ n))
    (indent n) (princ "HA: ") (princ (HA tm)) (nl)
    (print-machine (HA tm) (1+ n))

    #| calling fork inside print-machine is not a good idea
    (if (is-void tm)
      (progn
        (indent n) 
        (princ "region: #void")
        (nl)
        )
      (let(
            (tm0 (fork-0 tm))
            )
        (cue-leftmost tm0)
        (indent n) 
        (princ "region: ")
        (print-indicated tm0 tm)
        (⟳(λ(cont-loop cont-return)
            (s tm0
              (λ()
                (princ " ")
                (print-indicated tm0 tm)
                (funcall cont-loop)
                )
              cont-return
              )))
        (nl)
        ))
     |#

    (print-entanglements-0 tm n)
    )


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

  ;; 1. a parked region is not void, i.e. it has cells
  ;; 2. a region is part of the base space, hence any machine on the base space has cells
  ;; 3. location is on the base space, thus it has cells
  ;; 4. therefore, location can not be void.  (It might be parked or active.)
  ;;
  ;; 5. a parked location means that the region starts at leftmost of the base space.
  ;; 6. both parked machines and non-rightmost active machines can be stepped
  ;; 7. therefore, if we cue to location, and step, either rightmost continuation, or step happens
  ;; 8. only a void region can be located at rightmost (as regions lie to the right)
  ;; 9. note point 1
  ;; 10. therefore, stepping location will not go to a rightmost continuation
  ;;
  ;; 11. for a parked machine has a (HA tm) = ∅.  Hence we must create a new HA to become
  ;;     active.
  ;;
    (defmethod cue-leftmost-0  ((tm tm-region) (state parked) cont-ok cont-void)
      (let(
            (location (region-location (parameters tm)))
            )
        (setf (HA tm) (fork location))
        (s (HA tm) cont-ok #'cant-happen)
        (setf (state tm) active)
        ))

  ;; same reasoning as for parked state, though starting 'an active region is not void ...'
  ;;
    (defmethod cue-leftmost-0  ((tm tm-region) (state active) cont-ok cont-void)
      (let(
            (location (region-location (parameters tm)))
            )
        (cue-to-0 (HA tm) location)
        (s (HA tm) cont-ok #'cant-happen)
        (setf (state tm) active)
        ))

  ;; (HA tm) is already entangled, so we use cue-to-0
  ;; regions cue quickly to rightmost
  (defmethod cue-rightmost-0  ((tm tm-region) (state active) cont-ok cont-void) 
    (let(
          (rightmost-region (region-rightmost (parameters tm)))
          )
      (cue-to-0 (HA tm) rightmost-region)
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

  ;; alloc new leftmost
  (defmethod a◧-0
    (
      (tm tm-region)
      (state void)
      object 
      cont-ok
      cont-not-supported
      cont-no-alloc
      )
    (let(
          (location (region-location (parameters tm)))
          )
      ;; if location is initially void, upon success of #'a it moves to parked
      (a location object
        (λ()
          (let( ; we are going to need a rightmost pointer ..
                (rightmost (fork location)) ; rightmost is now either active or parked
                )
            (s rightmost #'do-nothing #'cant-happen) ; we just created a cell to right of location
            (setf (region-rightmost (parameters tm)) rightmost)
            (setf (state tm) parked)
            (funcall cont-ok)
            ))
        cont-not-supported
        cont-no-alloc
        )))

  (defmethod a◧-0
    (
      (tm tm-region)
      (state parked)
      object 
      cont-ok
      cont-not-supported
      cont-no-alloc
      )
    (let(
          (location (region-location (parameters tm)))
          )
      (a location object cont-ok cont-not-supported cont-no-alloc)
      ))

  (defmethod a◧-0
    (
      (tm tm-region)
      (state active)
      object 
      cont-ok
      cont-not-supported
      cont-no-alloc
      )
    (let(
          (location (region-location (parameters tm)))
          )
      (a location object cont-ok cont-not-supported cont-no-alloc)
      ))

  ;; alloc new rightmost
  (defmethod a◨-0 
    (
      (tm tm-region) 
      (state active) 
      object
      cont-ok
      cont-not-supported 
      cont-no-alloc
      )
    (as (region-rightmost (parameters tm)) object cont-ok cont-not-supported cont-no-alloc)
    )

  ;; same as for an active region
  (defmethod a◨-0 
    (
      (tm tm-region) 
      (state parked) 
      object
      cont-ok
      cont-not-supported 
      cont-no-alloc
      )
    (as (region-rightmost (parameters tm)) object cont-ok cont-not-supported cont-no-alloc)
    )

  ;; adding a new cell to the right of an empty tape, is the same as
  ;; adding a new cell to the left of an empty tape
  (defmethod a◨-0 
    (
      (tm tm-region) 
      (state void) 
      object
      cont-ok
      cont-not-supported 
      cont-no-alloc
      )
    (a◧-0 tm state object cont-ok cont-not-supported cont-no-alloc)
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

  (defmethod a-0
    (
      (tm tm-region)
      (state void)
      object 
      cont-ok
      cont-not-supported
      cont-no-alloc
      )
    (a◧-0 (HA tm) state object cont-ok cont-not-supported cont-no-alloc)
    )

  (defmethod a-0
    (
      (tm tm-region)
      (state parked)
      object 
      cont-ok
      cont-not-supported
      cont-no-alloc
      )
    (a◧-0 (HA tm) state object cont-ok cont-not-supported cont-no-alloc)
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

  ;; state will be active when this is called, there will be no collisions
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
  ;; 1. Entanglement accounting for the region is separate from that for the base machine.
  ;; Hence when we use a region we have two levels of entanglement.  Location, rightmost,
  ;; and HA are entangled on the base machine.  The tm-region is entangled with itself
  ;; and any copies made of it.
  ;;
  ;; 2. When we make a copy of a tm-region, we reference the same location and rightmost
  ;; objects that are referenced on the original machine.  However, we must create a new
  ;; HA, so that the copy may step independently.
  ;;
  ;; 3. In tm-derived-2.lisp, fork and cue-to each call cue-to-2 to do the work.  cue-to-2
  ;; then calls cue-to-0.  The purpose of cue-to-2 is to add the entanglement list to
  ;; tm-cued.
  ;; 
  ;; 4. in tm-derived-0.lisp, fork-0 calls cue-to-0 directly.  The purpose of fork-0 is to
  ;; provide an efficient fork in situations where entanglement accounting is simply not
  ;; not needed.  We have made extensive use of this in our library.
  ;;
  ;; By definition, cue-to-0 does not do entanglement accounting, it is part of a lower
  ;; abstraction level.  When we need entanglement accounting, that is added by cue-to-2.
  ;;
  ;; Here we define cue-to-0 for tm-region.  As cue-to-0 is making a copy, a new
  ;; HA must be made [see 2 above]. Here is where we run into an issue.  If we use
  ;; fork-0 for creating the new HA, then we have proper behavior for fork-0 of the
  ;; the tm-region, i.e. we don't do any entanglement accounting [see 4 above].  However,
  ;; when cue-to-0 is called as part of cue-to-2 [see 3 above],  cue-to-2 adds accounting
  ;; at the tm-region level, but not at the base machine level for HA - this is bad.
  ;;
  ;; If instead we use fork to create the new HA, we end up with entanglement accounting
  ;; for fork-0, which, again, is bad.
  ;;
  ;; the solution -> we will add dispatch to cue-to-2 against the tm type, so that
  ;; machines like tm-region can do proper entanglement accounting.
  ;;
    (defmethod cue-to-0
      (
        (tm-cued tm-region)
        (tm-orig tm-region)
        )
      (setf (state tm-cued) (state tm-orig))
      (setf (HA tm-cued) (fork-0 (HA tm-orig)))
      (setf (parameters tm-cued) (parameters tm-orig))
      tm-cued
      )

;;...    (defmethod cue-to-2
