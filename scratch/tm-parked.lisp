#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  We do not make tm-empty machines, rather tm-parked is a state
  a machine may be in.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
;;  we do not make tm-parked machines, rather this is the state that
;;  other machines come up in.


  (defclass tm-parked (tm-void)())


;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod s
    (
      (tm tm-parked)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (if 
      (tm tape)
      (progn
        (change-class tm (HA tm))
        (cue-leftmost tm)
        )
      (funcall cont-rightmost) ; = call next method
      ))

  (defmethod a
    (
      (tm tm-parked)
      object
      &optional
      cont-ok
      (cont-no-alloc (error 'tm-alloc-fail))
      )
    (if
      (tm tape)
      (let(
            (tm1 (dup tm))
            )
        (change-class tm1 (HA tm))
        (a◧ tm object cont-ok cont-no-alloc)
        )
      (funcall cont-no-alloc) ; = call next method
      ))

  (defmethod d 
    (
      (tm tm-parked)
      &optional 
      spill
      cont-ok
      (cont-rightmost (λ()(error 'tm-deallocation-request-at-rightmost)))
      cont-no-alloc
      )
    (if
      (tm tape)
      (let(
            (tm1 (dup tm))
            )
        (change-class tm1 (HA tm))
        (d◧ tm object spill cont-ok cont-no-alloc)
        )
      (funcall cont-rightmost) ; = call next method
      ))
