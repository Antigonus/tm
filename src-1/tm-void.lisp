#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

The void projective machine has the control mechanism for a tape, but any attempt to
access the tape or modify the tape has no effect and takes a continuation path.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-void (tape-machine)())

  (defmethod init 
    (
      (tm tm-void)
      init-list 
      &optional
      (cont-ok (be t))
      (cont-fail (λ()(error 'bad-init-value)))
      )
    (setf (state tm) void)
    (setf (HA tm) ∅)
    (setf (tape tm) ∅)
    (setf (parameters tm) ∅)
    (setf (entanglements tm) (make-entanglements tm))
    (funcall cont-ok)
    )

  (defmethod unmount ((tm tm-void) state)
    (declare (ignore tm state))
    ∅
    )

;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod r-0 (tm (state void) cont-ok cont-parked)
    (declare (ignore tm state cont-ok))
    (funcall cont-parked)
    )

  (defmethod w-0 (tm (state void) object cont-ok cont-parked)
    (declare (ignore tm state object cont-ok))
    (funcall cont-parked)
    )

  (defmethod cue-leftmost-0  (tm (state void) cont-ok cont-parked)
    (declare (ignore tm state cont-ok))
    (funcall cont-parked)
    )

  (defmethod heads-on-same-cell-0 (tm0 (state0 void) tm1 state1 cont-true cont-false cont-parked)
    (declare (ignore tm state cont-ok))
    (funcall cont-parked)
    )
  (defmethod heads-on-same-cell-0 (tm0 state0 tm1 (state1 void) cont-true cont-false cont-parked)
    (declare (ignore tm state cont-ok))
    (funcall cont-parked)
    )

  (defmethod s-0 (tm (state void) cont-ok cont-rightmost)
    (declare (ignore tm state cont-ok))
    (funcall cont-rightmost)
    )

  (defmethod a◧-0 ((tm tm-void) state object cont-ok cont-no-alloc)
    (declare (ignore tm state cont-ok))
    (funcall cont-no-alloc)
    )

  (defmethod a-0 ((tm tm-void) state object cont-ok cont-no-alloc)
    (declare (ignore tm state cont-ok))
    (funcall cont-no-alloc)
    )

  ;; deallocation of a whole tape returns no objects, so we can follow cont-ok
  (defmethod d*-0 (tm (state void) cont-ok cont-not-supported)
    (declare (ignore cont-not-supported))
    (funcall cont-ok)
    )
     
  ;; deallocation of a single cell returns the object that was in the cell,
  ;; but we can't do that in a void space, so we follow cont-not-supported
  (defmethod d◧-0 (tm (state void) cont-ok cont-not-supported)
    (declare (ignore cont-ok))
    (funcall cont-not-supported)
    )

  (defmethod d-0 (tm (state void) cont-ok cont-not-supported)
    (declare (ignore cont-ok))
    (funcall cont-not-supported)
    )

