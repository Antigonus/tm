#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

A tape machine is defined by giving definitions to these primitives.

CLOS, like many object systems, requires that the parent type signature be duplicated by
children.  However, in cases our child types will have more continuations than defined for
the parent.  Hence we include a &rest parameter that children may take advantage of for
adding more continuations.  These should be called out in a destructuring bind immediately
after the call.

Note that '⋯' is a single character that we use as a variable name for holding the rest
parameters.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defgeneric r (tm &rest ⋯))
  (defgeneric w (tm object &rest ⋯))

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defgeneric cue-leftmost (tm &rest ⋯))


;;--------------------------------------------------------------------------------
;; head location
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

;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defgeneric s
    (
      tm
      &optional 
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (:documentation
      "If the head is on a cell, and there is a right neighbor, puts the head on the
       right neighbor and cont-ok.  If there is no right neighbor, then cont-rightmost.
       ")
    )

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defgeneric a
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (:documentation
    "If no cells are available, cont-no-alloc.  Otherwise, allocate a new cell and place
     it to the right of the cell the head is currently on.  The newly allocated cell will
     be initialized with the given object.
     "
      ))
