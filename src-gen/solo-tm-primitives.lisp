#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Destrictuve operation primitives for solo machines.

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; cell allocation
;;

  ;; see tm-derived-1  for defun a◧
  ;; the job of this primitive is to add a new leftmost cell to the specified machine
  (defgeneric a◧-0 (tm state object cont-ok cont-not-supported cont-no-alloc))
  (defmethod a◧-0 (tm state object cont-ok cont-not-supported cont-no-alloc)
    (declare (ignore tm state object cont-ok cont-no-alloc))
    (funcall cont-not-supported)
    )
  (defmethod a◧-0 (tm (state abandoned) object cont-ok cont-not-supported cont-no-alloc)
    (declare (ignore tm object cont-ok cont-not-supported cont-no-alloc))
    (error 'operation-on-abandoned)
    )

  (defmethod a-0 ((tm tape-machine) (state void) object cont-ok cont-not-supported cont-no-alloc)
    (a◧-1 tm state object cont-ok cont-not-supported cont-no-alloc)
    )
  (defmethod a-0 ((tm tape-machine) (state parked) object cont-ok cont-not-supported cont-no-alloc)
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
    (defmethod d◧-0 (tm cont-ok cont-not-supported)
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
    (defmethod d-0 (tm cont-ok cont-not-supported)
      (declare (ignore cont-ok))
      (funcall cont-not-supported)
      )

;;--------------------------------------------------------------------------------
;; copying
;;
;; The base copying requies no entanglement accounting, because that is derived.
;; This is for internal use.
;;

  ;; see tm-derived-2 for cue-to
  (defgeneric cue-to-0
    (
      tm-cued ; object affected, contents get clobbered
      tm-orig ; remains undisturbed
      )
    (:documentation "Used internally to make copies that have no entanglement accounting.")
    )

  ;; see tm-derived-2 for cue-to
  ;; this will work for many machine types
  (defmethod cue-to-0
    (
      tm-cued 
      (tm-orig tape-machine)
      )
    (setf (state tm-cued) (state tm-orig))
    (setf (HA tm-cued) (HA tm-orig))
    (setf (tape tm-cued) (tape tm-orig))
    (setf (parameters tm-cued) (parameters tm-orig))
    tm-cued
    )

