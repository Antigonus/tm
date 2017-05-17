#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

A subspace is a tape held as an instance within a cell.  A subspace
may be entered using #'si from the cell.  Once a subspace has been entered, leftmost
and rightmost, etc. apply to the subspace (not the original tape).

This is copied from tm-region.lisp:

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
    then including in the cells of the manifold the instances which are subspaces.  We then
    either have to assume that every contained tape machine is a manifold, or we have to
    externally keep track of the fact if a machine found as an instance is a manifold that
    in turn holds multiple subspaces, or if it is directly a subspace.

    Hmm, we could also use manifolds to give order to multiple subspaces given at the
    same location, but the addresses in the manifold are simply a second integer in a
    pair of addresses - i.e. this is a fractional addressing scheme, though again,
    we need a bit to know if the fraction is present.  That bit could be the type
    of machine - a manifold machine type means the contents are a manifold.


|#

(in-package #:tm)

  (defun si 
    (
      tm
      &optional
      (cont-ok (be t))
      (cont-mount-failed (be ∅))
      )
    "If either: instance is a tm, or #'mk succeeds on the instance, steps in.
     Otherwise cont-mount-failed.
     "
    (r tm
      (λ(subspace)
        (if 
          (typep subspace 'tape-machine)
          (progn
            (recycle-entangled-with subspace tm)
            (funcall cont-ok)
            )
          (mount subspace
            (λ(new-tm) ; should mark new-tm readonly, except we don't support that yet
              (recycle-entangled-with tm new-tm)
              (funcall cont-ok)
              )
            cont-mount-failed
            )))
        cont-mount-failed
        ))
    
  (defun ai
    (
      tm
      instance
      &optional
      (cont-ok (be t))
      (cont-mount-fail (be ∅)) ; instance isn't a tape machine
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "Allocates a new cell to the left of leftmost in the subspace.
     "
    (r tm
      (λ(subspace)
        (if 
          (typep subspace 'tape-machine)
          (epa subspace instance cont-ok cont-no-alloc)
          cont-mount-fail
          ))
      cont-mount-fail
      ))

  (defun aisi
    (
      tm
      instance
      &optional
      (cont-ok (be t))
      (cont-mount-fail (be ∅)) 
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "#'ai then step into the subspace"
    (ai tm instance
      (λ()(recycle-entangled-with tm (r tm))(funcall cont-ok))
      cont-mount-fail
      cont-no-alloc
      ))

  (defun di (
              tm 
              &optional 
              spill 
              (cont-ok #'echo)
              (cont-mount-fail (λ()(error 'mount-fail)))
              (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
              (cont-not-supported (λ()(error 'not-supported)))
              (cont-collision (λ()(error 'dealloc-entangled)))
              (cont-no-alloc (λ()(error 'alloc-fail)))
              )
    "Object is subspace. Deletes the subspace leftmost."
    (r tm
      (λ(subspace)
        (if 
          (typep subspace 'tape-machine)
          (epd subspace spill cont-ok cont-rightmost cont-not-supported cont-collision cont-no-alloc)
          cont-mount-fail
          ))
      cont-mount-fail
      ))

    
