#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

A tape machine is defined by giving definitions to these primitives.

CLOS, like many instance systems, requires that the parent type signature be duplicated by
children.  However, in cases our child classes will have more continuations than defined for
the parent.  Hence we include a &rest parameter that children may take advantage of for
adding more continuations.  These should be called out in a destructuring bind immediately
after the call.

Note that '⋯' is a single unicode character which I employ as the name of the variable
that holds the rest list.

note esr stands for: entangled copy the iterator passed in, step the new iterator, and
read from it.  In otherwords, read the instance in the right neighbor cell. It was necessary
to make esr and esw primitive operations because, by definition, a region exists to the
right of the cell the head is on, and regions are native instances.  Note, that no entangled
copy operation is defined for the tm-primitive type, the 'e' is just part of the name of these
functions.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (def-function-class r (tm &rest ⋯))
  (def-function-class esr (tm &optional cont-ok cont-rightmost &rest ⋯))

  (def-function-class w (tm instance &rest ⋯))
  (def-function-class esw (tm instance &optional cont-ok cont-rightmost &rest ⋯))

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (def-function-class cue-leftmost (tm &rest ⋯))


;;--------------------------------------------------------------------------------
;; head stepping
;;
  (def-function-class s
    (
      tm
      &optional 
      cont-ok
      cont-rightmost
      &rest ⋯
      )
    (:documentation
      "If the head is on a cell, and there is a right neighbor, puts the head on the
       right neighbor and cont-ok.  If there is no right neighbor, then cont-rightmost.
       "))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (def-function-class a
    (
      tm
      instance
      &optional
      cont-ok
      cont-no-alloc
      &rest ⋯
      )
    (:documentation
    "If no cells are available, cont-no-alloc.  Otherwise, allocate a new cell and place
     it to the right of the cell the head is currently on.  The newly allocated cell will
     be initialized with the given instance.
     "))
      
;;--------------------------------------------------------------------------------
;; location
;;  
  (def-function-class on-leftmost 
    (
      tm
      &optional
      cont-true
      cont-false
      &rest ⋯
      )
    (:documentation
      "tm head is on leftmost.
      "))

  (def-function-class on-rightmost
    (
      tm
      &optional
      cont-true
      cont-false
      &rest ⋯
      )
    (:documentation
      "tm head is on the rightmost cell.
      "
      ))


