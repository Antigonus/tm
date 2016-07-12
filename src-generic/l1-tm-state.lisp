#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#
(in-package #:tm)


;;--------------------------------------------------------------------------------
;; general
;;
  (defun is-abandoned (tm) (eq (state tm) abandoned))
  (defun is-void (tm) (eq (state tm) void))
  (defun is-parked (tm) (eq (state tm) parked))
  (defun is-active (tm) (eq (state tm) active))

;;--------------------------------------------------------------------------------
;; abandon
;;
;;
  (defun abandon (tm)
    "Abandons a machine. It is an error to operate on an abandoned machine."
    (abandon-0 tm (state tm))
    )

  (defgeneric abandon-0 (tm tm-state))
  (defmethod abandon-0 ((tm tape-machine) (tm-state abandoned))
    )
  (defmethod abandon-0 ((tm tape-machine) (tm-state void))
    (setf (state tm) abandoned)
    )
  (defmethod abandon-0 ((tm tape-machine) (state parked))
    (setf (state tm) abandoned)
    (setf (tape tm) ∅) ; voids the tape
    )
  (defmethod abandon-0 ((tm tape-machine) (state active))
    (setf (state tm) abandoned)
    (setf (HA tm) ∅) ; parks the head
    (setf (tape tm) ∅) ; voids the tape
    )

;;--------------------------------------------------------------------------------
;; void
;;
;;  Voiding affects all machines in an entanglement group. It can not be done
;;  on non-destructive machines.  It can be done on solo machines. Hence, it
;;  can not be defined here in the parent.
;;


;;--------------------------------------------------------------------------------
;; park
;;
;;  In an entanglement group parking only affects the machine that park is called on.
;;
  ;; parks the machine
  (defun park (tm &optional (cont-ok (be t)) (cont-void (be ∅)))
    "parks the head"
    (park-0 tm (state tm) cont-ok cont-void)
    )
  (defgeneric park-0 (tm state cont-ok cont-void))
  (defmethod park-0 ((tm tape-machine) (state abandoned) cont-ok cont-void)
    (declare (ignore tm state cont-ok cont-void))
    (error 'operation-on-abandoned)
    )
  (defmethod park-0 ((tm tape-machine) (state void) cont-ok cont-void)
    (declare (ignore tm state cont-ok))
    (funcall cont-void)
    )
  (defmethod park-0 ((tm tape-machine) (state parked) cont-ok cont-void)
    (declare (ignore tm state cont-void))
    (funcall cont-ok)
    )
  ;; this will work for many tm types
  (defmethod park-0 ((tm tape-machine) (state active) cont-ok cont-void)
    (declare (ignore state cont-void))
    (setf (state tm) parked)
    (setf (HA tm) ∅)
    (funcall cont-ok)
    )

;;--------------------------------------------------------------------------------
;; An abandoned machine can not be made active, or have its state changed.
;; A void machine can be made parked, by allocating a cell to it.
;; cue-leftmost (or cue-rightmost) will make a parked machine active.
;;
