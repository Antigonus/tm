#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#
(in-package #:tm)


;;--------------------------------------------------------------------------------
;; general
;;
  (defun is-parked (tm) (eq (state tm) parked))
  (defun is-void (tm) (eq (state tm) void))
  (defun is-active (tm) (eq (state tm) active))
  (defun is-abandoned (tm) (eq (state tm) abandoned))

;;--------------------------------------------------------------------------------
;; parking
;;
  ;; parks the machine
  (defun park (tm &optional (cont-ok (be t)) (cont-void (be ∅)))
    "parks the head"
    (park-0 tm (state tm) cont-ok cont-void)
    )
  (defgeneric park-0 (tm state cont-ok cont-void))
  (defmethod park-0 ((tm nd-tm-machine) (state void) cont-ok cont-void)
    (declare (ignore tm state cont-ok))
    (funcall cont-void)
    )
  (defmethod park-0 ((tm nd-tm-machine) (state parked) cont-ok cont-void)
    (declare (ignore tm state cont-void))
    (funcall cont-ok)
    )
  ;; this will work for many tm types
  (defmethod park-0 ((tm nd-tm-machine) (state active) cont-ok cont-void)
    (declare (ignore state cont-void))
    (setf (HA tm) ∅)
    (setf (state tm) parked)
    (funcall cont-ok)
    )


