#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

A tape machine is defined by giving definitions to these primitives.

'➜' means 'continuation', '➜' without a label after is a list of continuation functions,
one of which is selected upon exit.

'esr' stands for: entangled copy the iterator passed in, step the new iterator, and
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
  (def-function-class r (tm &optional ➜))
  (def-function-class esr (tm &optional ➜))

  (def-function-class w (tm instance &optional ➜))
  (def-function-class esw (tm instance &optional ➜))

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (def-function-class cue-leftmost (tm &optional ➜))


;;--------------------------------------------------------------------------------
;; head stepping
;;
  (def-function-class s (tm &optional ➜)
    (:documentation
      "If the head is on a cell, and there is a right neighbor, puts the head on the
       right neighbor and cont-ok.  If there is no right neighbor, then cont-rightmost.
       "))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (def-function-class a (tm instance &optional ➜)
    (:documentation
    "If no cells are available, cont-no-alloc.  Otherwise, allocate a new cell and place
     it to the right of the cell the head is currently on.  The newly allocated cell will
     be initialized with the given instance.
     "))
      
;;--------------------------------------------------------------------------------
;; location
;;  
  (def-function-class on-leftmost (tm &optional ➜)
    (:documentation
      "tm head is on leftmost ➜t, else ➜∅
      "))

  (def-function-class on-rightmost (tm &optional ➜)
    (:documentation
      "tm head is on the rightmost cell ➜t, else ➜∅
      "))


