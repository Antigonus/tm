#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

The void projective machine has the control mechanism for a tape, but any attempt to
access the tape or modify the tape has no effect and takes a continuation path.

The tm-void machine is the limitating condition of a tm-array with no elements. Unlike a
fixed array machine with no elements, tm-void goes to ∅ upon being unmounted, rather than
to #'().

Other machines may also be in the void state, but those will leave the void state when
cells are allocated to them. Howeer, like a fixed array machine, tm-void does not support
allocation.  

A void machine will be in the void state, and thus most of the methods are already
covered by the primitive void state specializations.


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
    (declare (ignore cont-fail))
    (setf (state tm) void)
    (setf (HA tm) ∅)
    (setf (tape tm) ∅)
    (setf (parameters tm) ∅)
    (setf (entanglements tm) (make-entanglements tm))
    (funcall cont-ok)
    )

  (defmethod unmount-0 ((tm tm-void) state)
    (declare (ignore tm state))
    ∅
    )

;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod a◧-0 ((tm tm-void) state object cont-ok cont-not-supported cont-no-alloc)
    (declare (ignore tm state cont-ok cont-no-alloc))
    (funcall cont-not-supported)
    )

  (defmethod a-0 ((tm tm-void) state object cont-ok cont-not-supported cont-no-alloc)
    (declare (ignore tm state cont-ok cont-no-alloc))
    (funcall cont-not-supported)
    )
     


