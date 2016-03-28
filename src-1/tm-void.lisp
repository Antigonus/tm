#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  The tape machine version of the bit bucket ...

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-void (tape-machine)())

  (defmethod tm-init
    (
      (instance tm-void)
      &optional
      init 
      (cont-ok #'echo) 
      cont-fail
      )
    (declare (ignore init cont-fail))
    (funcall cont-ok instance)
    )

;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod r ((tm tm-void)) ∅)

  (defmethod w ((tm tm-void) object)
    (declare (ignore tm object))
    t
    )

  (defmethod cue-leftmost  ((tm tm-void)) 
    tm
    )

  (defun heads-on-same-cell-void-0 (tm0 tm1 cont-true cont-false)
    (if
      (∧
        (typep tm0 'tm-void)
        (typep tm1 'tm-void)
        )
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defmethod heads-on-same-cell 
    (
      (tm0 tm-void) 
      (tm1 tape-machine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-void-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod heads-on-same-cell 
    (
      (tm0 tape-machine) 
      (tm1 tm-void) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-void-0 tm0 tm1 cont-true cont-false)
    )

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
  
  (defmethod a
    (
      (tm tm-void)
      object
      &optional
      cont-ok
      cont-no-alloc
      )
    (declare (ignore tm object cont-no-alloc))
    (funcall cont-ok)
    )

  (defmethod d 
    (
      (tm tm-void)
      &optional 
      spill
      (cont-ok #'echo)
      cont-rightmost
      cont-no-alloc
      )
    (declare (ignore tm cont-rightmost))
    (a spill ∅
      (λ() (funcall cont-ok ∅))
      cont-no-alloc
      ))
