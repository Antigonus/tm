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
  (defmethod a◧-0 ((tm tm-void) state object cont-ok cont-no-alloc)
    (declare (ignore tm state cont-ok))
    (funcall cont-no-alloc)
    )

  (defmethod a-0 ((tm tm-void) state object cont-ok cont-no-alloc)
    (declare (ignore tm state cont-ok))
    (funcall cont-no-alloc)
    )
     


