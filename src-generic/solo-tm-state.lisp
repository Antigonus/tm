#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; void
;;
;; solo machines do not have entanglement groups, so we can void a given machine
;; without concern that others may be sharing the tape.
;;
;;
  (defun void (tm)
    "Voids a machine."
    (void-0 tm (state tm))
    )

  (defgeneric void-0 (tm tm-state))
  (defmethod void-0 ((tm tape-machine) (tm-state abandoned))
    (error 'operation-on-abandoned)
    )
  (defmethod void-0 ((tm tape-machine) (tm-state void))
    )
  (defmethod void-0 ((tm tape-machine) (state parked))
    (setf (state tm) void)
    (setf (tape tm) ∅) ; voids the tape
    )
  (defmethod void-0 ((tm tape-machine) (state active))
    (setf (state tm) void)
    (setf (HA tm) ∅) ; parks the head
    (setf (tape tm) ∅) ; voids the tape
    )

