#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

The void projective machine has the control mechanism for a tape,
but any attempt to read or write it is an error.

(HA tm) holds the type for tape space.

A tm-void machine takes as an initialization value a type for the
tape space.

Calling alloc, #'a, will cause the machine to transition to 'tm-parked-singular.

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
    (destructuring-bind
      (&key tm-type mount &allow-other-keys) init-list
      (cond
        (mount (funcall cont-fail))
        (tm-type
          (setf (HA tm) tm-type)
          (setf (tape tm) ∅)
          (funcall cont-ok)
          )
        (t
          (setf (HA tm) 'tm-void)
          (setf (tape tm) ∅)
          (funcall cont-ok)
          ))))

;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod r ((tm tm-void))
    (declare (ignore tm))
    (error 'oid-access)
    )
  (defmethod w ((tm tm-void) object)
    (declare (ignore tm object))
    (error 'void-access)
    )

  (defmethod cue-leftmost (tm) t)

  (defun heads-on-same-cell-void-0 (tm0 tm1 cont-true cont-false)
    (if
      (∧
        (typep tm0 'tm-void)
        (typep tm1 'tm-void)
        (eq (HA tm0) (HA tm1))
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

  (defmethod s
    (
      (tm tm-void)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (declare (ignore cont-ok))
    (funcall cont-rightmost)
    )

  (defmethod a
    (
      (tm tm-void)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (let(
          (tm-type (HA tm))
          )
      (if
        (eq tm-type 'tm-void)
        (funcall cont-no-alloc)
        (progn
          (change-class tm 'tm-parked-singular)
          (init tm {:tm-type tm-type :mount object} 
            (λ()(funcall cont-ok)) 
            (λ()(error 'impossible-to-get-here))
            )))))

  (defmethod d 
    (
      (tm tm-void)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-no-dealloc (λ()(error 'dealloc-fail)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (declare (ignore tm spill cont-ok cont-no-alloc))
    (funcall cont-no-dealloc)
    )

  (defmethod d◧
    (
      (tm tm-void)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-no-dealloc (λ()(error 'dealloc-fail)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (declare (ignore tm spill cont-ok cont-no-alloc))
    (funcall cont-no-dealloc)
    )

  (defmethod unmount ((tm tm-void))
    (case (HA tm)
      (tm-list '())
      (tm-array #())
      (t (error 'can-not-unmount))
      ))
      
