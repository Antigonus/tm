#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

A solo tape machine is the sole user of its tape.  There are no other heads on the 
tape, save for that of the solo machine.  Consequently one does not have to worry
about either collisions or other machines having a reference to the same tape.

Consequently, destructive operations are allowed on solo machines.  

Even with these destructive operations, solo machines can not be recycle-entangled or
mk-entangled.  This is because such operations would cause the tape to become shared,
and thus negate our no collisions and no shared tape reference properites.

Without entangled copyies, we can not make temporary variables that have independent head
movement from the machine they were copied from.  This prevents us from implementing some
derived methods that exist for nd-tape-machines.

ellocate by value conundrum:

Without temporary variables, we do not have the indexed read function.  So we can not know
what is in the next cell until we visit that cell.  However, by the time we visit a cell,
it is too late to deallocate the cell, as the deallocation function effects the right
neighbor cell. We might step right, read, step left, delete, except that single linked
lists do not support step left. We can't cue leftmost and then walk up and stopping short
by one - thus emulating step left - because we have no way of marking a cell to stop at.
And even if we were able to do this, it would be compuntationally inefficient.

Though we do not have a general read index function, we can implement a function that
specifically reads the right neighbor instead of the current cell.  With streams such
a read is called 'peek'.  With the tape we will call it 'esr'  where 'e' means to 
create an entangled copy, step, then read.  Though we will not make such a copy.
Recall that 'esnr' is the general indexed read.  'esr' can be implemented in
tape machine, so we will do so there.

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; cell allocation
;;
  ;; add a new leftmost
  (defgeneric a◧
    (
      tm
      object
      &optional
      cont-ok ;  (be t)
      cont-no-alloc ; #'alloc-fail)
      )
    (:documentation
      "Allocates a cell to the left of leftmost (thus becoming the new leftmost).
      "
      ))


;;--------------------------------------------------------------------------------
;; cell deallocation
;;
;; Spill can be ∅, in which case we just drop the deallocated cell.  When spill is not ∅,
;; then the deallocated cell is moved to spill, or a new allocation is made on spill and
;; the object from the deallocated cell is moved to it, preferably the former. 
;;
;; d must have transactional behavior, i.e. the cell is only dealloced if all goes well,
;; otherwise d makes no structural changes.  E.g. d will fail if spill is not nil, and
;; reallocation to spill fails
;;
  (defgeneric d (
                  tm 
                  &optional 
                  spill 
                  cont-ok ; #'echo
                  cont-rightmost ;(λ()(error 'dealloc-on-rightmost))
                  cont-no-alloc ;#'alloc-fail
                  &rest ⋯
                  )
    (:documentation
      "Deallocate the right neighbor of the cell the head is on.
       I.e. Deallocates a region of length 1 located to the right of the head.
       Returns the object from the deallocated cell.
       If spill is not ∅, the deallocated cell is moved to spill, or a new
       cell is allocated to spill and the object reference is moved there.
      "
      ))

  (defgeneric d◧ (
                   tm 
                   &optional 
                   spill 
                   cont-ok ; #'echo
                   cont-collision ; (λ()(error 'dealloc-collision))
                   cont-no-alloc ; #'alloc-fail
                   )
    (:documentation
      "Deallocates leftmost.
       Returns the object from the deallocated cell.
       If spill is not ∅, the deallocated cell is moved to spill, or a new
       cell is allocated to spill and the object reference is moved there.
      "
      ))



