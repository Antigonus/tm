#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These methods may cause state changes:
  park
  void

  see tm-derived-1:
  a◧ when the tm is void, transitions to parked
  d◧ when the tm is singleton, and the all entangled machines are parked, transitions to void

|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; tape machine states
;;

  ;; parks the machine
  (defun park (tm &optional (cont-ok (be t)) (cont-void (be ∅)))
    "parks the head"
    (park-0 tm (state tm) cont-ok cont-void)
    )
  (defgeneric park-0 (tm state cont-ok cont-void))
  (defmethod park-0 (tm (state void) cont-ok cont-void)
    (declare (ignore tm state cont-ok))
    (funcall cont-void)
    )
  (defmethod park-0 (tm (state parked) cont-ok cont-void)
    (declare (ignore tm state cont-void))
    (funcall cont-ok)
    )
  ;; this will work for many tm types
  (defmethod park-0 (tm (state active) cont-ok cont-void)
    (declare (ignore state cont-void))
    (setf (HA tm) ∅)
    (setf (state tm) parked)
    (funcall cont-ok)
    )

  ;; are all entangled machines parked? 
  (defun ∀-parked (tm &optional (cont-true (be t)) (cont-false (be ∅)))  
    (let(
          (es (entanglements tm))
          )
      (when (¬ es) (return-from ∀-parked (funcall cont-true)))
      (cue-leftmost es)
      (∀ es (λ(es)(eq (state (r es)) parked))
        cont-true
        cont-false
        )))

  (defun void (tm)
    "voids the machine"
    (void-0 tm (state tm))
    (let(
          (es (entanglements tm))
          )
      (when es
        (cue-leftmost es)
        (∀ es
          (λ(es)
            (unless (eq (r es) tm) (void-0 (r es) (state (r es))) t)
            )))))
  (defgeneric void-0 (tm state))
  (defmethod void-0 (tm (state void))
    (declare (ignore tm state))
    )
  ;; this will work for many tm types
  (defmethod void-0 (tm (state parked))
    (declare (ignore state))
    (setf (tape tm) ∅)
    (setf (state tm) void)
    )
  ;; this will work for many tm types
  (defmethod void-0 (tm (state active))
    (declare (ignore state))
    (setf (HA tm) ∅) ; parks the head
    (setf (tape tm) ∅) ; voids the tape
    (setf (state tm) void)
    )

  (defun is-parked (tm) (eq (state tm) parked))
  (defun is-void (tm) (eq (state tm) void))
  (defun is-active (tm) (eq (state tm) active))


  ;; If there exists an entangled machine that is void, then all entangled machines are
  ;; void. If there exists an entangled machine that is singleton, then all entangled
  ;; machines are singleton.
