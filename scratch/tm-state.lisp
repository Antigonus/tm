#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Note:
  a◧ when the tm is void, transitions to parked
  d◧ when the tm is singular, and the all entangled machines are parked, transitions to void
  both of these methods are destructive


|#
(in-package #:tm)


;;--------------------------------------------------------------------------------
;; parking
;;

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

;;--------------------------------------------------------------------------------
;; void
;;
;;
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


