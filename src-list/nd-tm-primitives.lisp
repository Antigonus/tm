#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Additional non-destructive primitives.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; copying
;;  
  (defgeneric mk-entangled (tm-orig)
    (:documentation
      "Make a new tape machine. Initializes the new machine by entangling it
       with tm-orig.  An entangled machine shares a tape, but has an independent
       head. Returns the new machine.
       "
      ))

  (defgeneric recycle-entangled (tm-to-be-recycled tm-orig)
    (:documentation
      "Like mk-entangled, but we recycle tm-to-be-recycled rather than creating
       a new instance.
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
    
