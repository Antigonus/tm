#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


Destructive operations are allowed on solo machines.

Even with these destructive operations, solo machines can not be recycle-entangled-with or mk-entangled-with.
This is because copy operations would cause the tape to become shared.  Without copying,
we can not make temporary variables that have independent head movement from the machine
they were copied from.  This prevents us from implementing some derived methods that 
exist for nd-tape-machines.


|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defmethod a-0 ((tm solo-tape-machine) (state void) object cont-ok cont-no-alloc)
    (a◧-0 tm state object cont-ok cont-no-alloc)
    )
  (defmethod a-0 ((tm solo-tape-machine) (state parked) object cont-ok cont-no-alloc)
    (a◧-0 tm state object cont-ok cont-no-alloc)
    )

  ;; see tm-derived-1  for defun a◧
  ;; the job of this primitive is to add a new leftmost cell to the specified machine
  (defgeneric a◧-0 (tm state object cont-ok cont-no-alloc))

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
  ;; see tm-derived-1 for defun d-1
  ;; when this is called:
  ;;    state is active
  ;;    there are no collisions
  ;;
    (defgeneric d-0 (tm spill cont-ok cont-rightmost cont-no-alloc))

  ;; see tm-derived-1 for defun d◧-1
  ;; when this is called:
  ;;    state is parked or active
  ;;    there are no collisions
  ;;
    (defgeneric d◧-0 (tm spill cont-ok cont-rightmost cont-no-alloc))


    


