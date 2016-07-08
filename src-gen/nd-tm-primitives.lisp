#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Primitives are divided between non-destructive operations, and destructive ones.  This
file only defines non-destructive primitives.  Destructive primitives invoke entangelment
accounting, and that accounting is defined on top of the non-destructive primitives.
Destructive primitives include allocating a new cell to the left of a tape, and
deallocating cells anywhere from the tape.

Fork and Cue-to are also defined with the destrucive primitives because they invoke
entanglement accounting.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; copying
;;  
  (defgeneric cue-to-0 (tm-cued tm-orig tm-orig-state))

  (defmethod cue-to-0 (tm-cued (tm-orig nd-tape-machine) (tm-orig-state abandoned))
    (declare (ignore tm-cued tm-orig tm-orig-state))
    (error 'operation-on-abandoned)
    )

  ;; this will work for many machines
  (defmethod cue-to-0 (tm-cued (tm-orig nd-tape-machine) tm-orig-state)
    (change-class tm-cued (type-of tm-orig))
    (setf (state tm-cued) (state tm-orig))
    (setf (HA tm-cued) (HA tm-orig))
    (setf (tape tm-cued) (tape tm-orig))
    (setf (parameters tm-cued) (parameters tm-orig))
    tm-cued
    )

  (defgeneric mk-cue-to-0 (tm-orig tm-orig-state))

  (defmethod mk-cue-to-0 ((tm-orig nd-tape-machine) (tm-orig-state abandoned))
    (declare (ignore tm-cued tm-orig tm-orig-state))
    (error 'operation-on-abandoned)
    )

  (defmethod mk-cue-to-0 ((tm-orig nd-tape-machine) tm-orig-state)
    (let(
          (instance (make-instance (type-of tm-orig)))
          )
      (cue-to-0 instance tm-orig tm-orig-state)
      ))

