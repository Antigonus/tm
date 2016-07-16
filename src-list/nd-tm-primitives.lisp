#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

non-destructive operation primitives

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

;;--------------------------------------------------------------------------------
;; head location
;;
;;  having two machines to compare means that we must have made a copy at some point
;;
  (defgeneric heads-on-same-cell
    (
      tm0
      tm1 
      &optional
      cont-true
      cont-false
      &rest ⋯
      ))
    
