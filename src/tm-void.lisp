#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  The tape machine version of the bit bucket ...

|#

(in-package #:le)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-void (tape-machine)())
  (defun mk-tm-void (&optional init (cont-ok #'echo) cont-fail)
    (declare (ignore init cont-fail))
    (funcall cont-ok (make-instance 'tm-void))
    )
  (mk-tm-hook 'tm-void #'mk-tm-void)

;;--------------------------------------------------------------------------------
;; essential methods
;;
  (defmethod r ((tm tm-void)) (error 'read-on-void))

  (defmethod w ((tm tm-void) object)
    (declare (ignore tm object))
    t
    )

  (defmethod cue-leftmost  ((tm tm-void)) 
    (declare (ignore tm))
    t
    )

  (defun tms-on-same-cell-void-0 (tm0 tm1 cont-true cont-false)
    (if
      (∧
        (typep tm0 'tm-void)
        (typep tm1 'tm-void)
        )
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defmethod tms-on-same-cell 
    (
      (tm0 tm-void) 
      (tm1 tape-machine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (tms-on-same-cell-void-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod tms-on-same-cell 
    (
      (tm0 tape-machine) 
      (tm1 tm-void) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (tms-on-same-cell-void-0 tm0 tm1 cont-true cont-false)
    )

  (defun test-tms-on-same-cell-void-0 ()
    (let(
          (a (mk-tm-void))
          (b (mk-tm-void))
          (c (make-instance 'tape-machine))
          )
      (setf (HA c) 1)
      (setf (tape c) 2)
      (∧
        (tms-on-same-cell a b)
        (¬ (tms-on-same-cell a c))
        (¬ (tms-on-same-cell c a))
        )))
   (test-hook test-tms-on-same-cell-void-0)

  ;; rightmost is true if the head is on a rightmost cell
  ;; void doesn't have cells so rightmost can't be true
  ;; as we are either rightmost or ok, we must be ok ;-)
  (defmethod s
    (
      (tm tm-void)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (declare (ignore cont-rightmost))
    (funcall cont-ok)
    )

  (defmethod a ((tm tm-void) object &optional cont-ok cont-no-alloc)
    (declare (ignore tm object cont-ok cont-no-alloc))
    t
    )

  (defmethod -a◧-s ((tm tm-void) object &optional cont-ok cont-no-alloc)
    (declare (ignore tm object cont-ok cont-no-alloc))
    t
    )
  
  ;; we don't spill on the grounds that there is nothing to spill
  (defmethod d 
    (
      (tm tm-void)
      &optional 
      spill
      (cont-ok (be t))
      cont-rightmost
      cont-no-alloc
      )
    (declare (ignore tm spill cont-rightmost cont-no-alloc))
    (funcall cont-ok)
    )

  ;; we don't spill on the grounds that there is nothing to spill
  (defmethod ◧d 
    (
      (tm tm-void)
      &optional 
      spill
      (cont-ok (be t))
      cont-rightmost
      cont-no-alloc
      )
    (declare (ignore tm spill cont-rightmost cont-no-alloc))
    (funcall cont-ok)
    )

  (defmethod m 
    (
      (tm tm-void)
      fill
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅)) ; rightmost of fill
      )
    (declare (ignore tm))
    (s fill
      cont-ok
      cont-rightmost
      ))



