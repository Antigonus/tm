#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


Destructive operation primitives for solo machines.

Even with these destructive operations, solo machines can neither be forked nor
cue-to'ed. This is because copy operations would cause the tape to become shared.
Without copying, we can not make temporary variables that have independent head
movement from the machine they were copied from.  This prevents us from implementing
some derived methods that do not have side effects,

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; cell allocation
;;
  ;; see tm-derived-1  for defun a◧
  ;; the job of this primitive is to add a new leftmost cell to the specified machine
  (defgeneric a◧-0 (tm state object cont-ok cont-not-supported cont-no-alloc))

  ;; default behavior, this should be implemented more specifically
  (defmethod a◧-0 ((tm solo-tape-machine) state object cont-ok cont-not-supported cont-no-alloc)
    (declare (ignore tm state object cont-ok cont-no-alloc))
    (funcall cont-not-supported)
    )

  (defmethod a-0 ((tm solo-tape-machine) (state void) object cont-ok cont-not-supported cont-no-alloc)
    (a◧-1 tm state object cont-ok cont-not-supported cont-no-alloc)
    )
  (defmethod a-0 ((tm solo-tape-machine) (state parked) object cont-ok cont-not-supported cont-no-alloc)
    (a◧-1 tm state object cont-ok cont-not-supported cont-no-alloc)
    )

;;--------------------------------------------------------------------------------
;; cell deallocation
;;
;; Spill can be ∅, in which case we just drop the deallocated cell.  When spill is not ∅,
;; then the deallocated cell is moved to spill, or a new allocation is made on spill and
;; the object from the deallocated cell is moved to it, preferably the former. 
;;
;; d must have transactional behavior, i.e. the cell is only dealloced if all goes well,
;; otherwise d makes no structural changes. 
;;
;; There are multiple reasons deallocation might fail a) because there is nothing
;; to deallocate,  b) because the tape does not support structural changes c) because
;; a machine has a head on the dealloc cell.
;;
;; d will also fail if spill is not nil, and reallocation to spill fails
;;
;; Entanglement accounting complicates the swap trick for implementing d◧, so I have made
;; it a primitive.
;;
  ;; see tm-derived-1 for defun d◧-1
  ;; when this is called:
  ;;    state will be parked or active
  ;;    there will be no collisions
  ;;
    (defgeneric d◧-0 (tm cont-ok cont-not-supported))

    ;; default behavior is to say the operation is not supported
    (defmethod d◧-0 ((tm solo-tape-machine) cont-ok cont-not-supported)
      (declare (ignore cont-ok))
      (funcall cont-not-supported)
      )

  ;; see tm-derived-1 for defun d-1
  ;; when this is called:
  ;;    state must be active
  ;;    there will be no collisions
  ;;
    (defgeneric d-0 (tm cont-ok cont-not-supported))
    
    ;; default behavior is to say the operation is not supported
    (defmethod d-0 ((tm solo-tape-machine) cont-ok cont-not-supported)
      (declare (ignore cont-ok))
      (funcall cont-not-supported)
      )


