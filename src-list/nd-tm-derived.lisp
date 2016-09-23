#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These functions derived from non-destructive primitives. 

There is no functional need for a new tape machine implementation to specialize these
functions.  

I wonder if many of these should be made into regular functions so as to improve
performance.

|#
(in-package #:tm)


;;--------------------------------------------------------------------------------
;; copying
;;  
  (defgeneric mk-entangled (tm-orig)
    (:documentation
      "Make a new tape machine. Initializes the new machine by entangling it with tm-orig.
       An entangled machine shares a tape, but has an independent head. Returns the new
       machine. See also #'with-mk-entangled which provides an entangled machine with
       limited scope (that is commonly what is needed).
       "
      ))

  (defmethod mk-entangled
    (
      (tm-orig nd-tape-machine)
      )
    (let(
          (tm1 (make-instance (type-of tm-orig)))
          )
      (init-entangled tm1 tm-orig)
      tm1
      ))


  (defgeneric recycle-entangled (tm-orig tm-to-be-recycled)
    (:documentation
      "Like mk-entangled, but we recycle the tm-to-be-recycled machine rather than
       creating a new instance. Typically this is used to keep the tm-to-be-recycled
       machine reference valid while walking across a collection of machines.  E.g. it is
       used in this manner in nd-tm-subspace.lisp. Though unnecessary, as we will already
       have a reference to tm-to-be-recycled, it is returned.  This keeps the
       recycled-entangled from consistent with the mk-entangled form.
       "
      ))

  (defmethod recycle-entangled
    (
      (tm-orig nd-tape-machine)
      tm1 ; tm1 is to be recycled
      )
    (change-class tm1 (type-of tm-orig))
    (init-entangled tm1 tm-orig)
    tm1
    )


  (defgeneric with-mk-entangled (tm continuation)
    (:documentation
      "Calls continuation with a locally scoped entangled copy of tm.
       "))

  ;; this becomes more interesting when we have entanglement accounting
  (defmethod with-mk-entangled
    (
      (tm nd-tape-machine)
      continuation
      )
    (let(
          (tm1 (mk-entangled tm))
          )
      (funcall continuation tm1)
      ))


;;--------------------------------------------------------------------------------
;; leftmost read and write
;;
  (defgeneric r◧ (tm &rest ⋯)
    (:documentation
      "Read leftmost.
      "
      ))

  (defmethod r◧ ((tm nd-tape-machine) &rest ⋯)
    (declare (ignore ⋯))
    (with-mk-entangled tm
      (λ(tm1)
        (cue-leftmost tm1)
        (r tm1) 
        )))

  (defgeneric w◧ (tm object &rest ⋯)
    (:documentation
      "Write leftmost.
      "
      ))

  (defmethod w◧ ((tm nd-tape-machine) object &rest ⋯)
    (declare (ignore ⋯))
    (with-mk-entangled tm
      (λ(tm1)
        (cue-leftmost tm1)
        (w tm1 object) 
        )))

;;--------------------------------------------------------------------------------
;; stepping with a boundary, boundaries are inclusive
;;
  ;; although we don't make any copies in this function, we do have two tape
  ;; machines that are on the same tape.  That can not happen with a solo machine
  ;; so nd-tape-machine is as far up the inheritance tree that this can go.
  (defgeneric s≠ 
    (
      tm0
      tm1
      &optional
      cont-ok
      cont-bound
      cont-rightmost
      )
    (:documentation
      "tm0 and tm1 are on the same tape. 
       If tm0's head is on the same call as tm1's head, take cont-bound.  Otherwise
       if tm0's head is on the rightmost cell, take cont-rightmost.  Otherwise,
       step tm0.
      "
      ))

  (defmethod s≠ 
    (
      (tm0 nd-tape-machine)
      (tm1 nd-tape-machine)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      (cont-bound (be ∅))
      )
    (heads-on-same-cell tm0 tm1
      cont-bound
      (λ()(s tm0 cont-ok cont-rightmost))
      ))


;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defgeneric a◨
    (
      tm
      object
      &optional
      cont-ok
      cont-no-alloc
      )
    (:documentation
      "Allocates a cell to the right of rightmost (thus becoming the new rightmost).
      "
      ))

  (defmethod a◨
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "Allocates a cell to the right of rightmost (thus becoming the new rightmost)."
    (with-mk-entangled tm
      (λ(tm1)
        (cue-rightmost tm1)
        (a tm1 object cont-ok cont-no-alloc)
        )))
