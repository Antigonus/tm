#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

The empty projective machine has the control mechanism for a tape,
but any attempt to read or write it is an error.

A tm-empty machine takes as an initialization value a type for the
tape space it would grow into should a new cell be allocated.

Calling alloc, #'a, will cause the machine to transition to 'tm-parked-tape.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-empty (tape-machine)())

  (defmethod init 
    (
      (tm tm-empty)
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
          (setf (HA tm) 'tm-empty)
          (setf (tape tm) ∅)
          (funcall cont-ok)
          ))))

  (defmethod unmount ((tm tm-empty))
    (case (HA tm)
      (tm-list ∅)
      (tm-array #())
      (t (error 'can-not-unmount))
      ))

  ;; no need to do anything
  ;; don't know if any compilers will freak with an empty body, so I put t
  (defmethod park ((tm tm-empty)) t)


;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod r ((tm tm-empty))
    (declare (ignore tm))
    (error 'oid-access)
    )
  (defmethod w ((tm tm-empty) object)
    (declare (ignore tm object))
    (error 'empty-access)
    )

  (defmethod cue-leftmost (tm) t)

  (defun heads-on-same-cell-empty-0 (tm0 tm1 cont-true cont-false)
    (if
      (∧
        (typep tm0 'tm-empty)
        (typep tm1 'tm-empty)
        (eq (HA tm0) (HA tm1))
        )
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defmethod heads-on-same-cell 
    (
      (tm0 tm-empty) 
      (tm1 tape-machine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-empty-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod heads-on-same-cell 
    (
      (tm0 tape-machine) 
      (tm1 tm-empty) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-empty-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod s
    (
      (tm tm-empty)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (declare (ignore cont-ok))
    (funcall cont-rightmost)
    )

  (defmethod a
    (
      (tm tm-empty)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    (let(
          (tm-type (HA tm))
          )
      (change-class tm 'tm-parked-tape)
      (init tm {:tm-type tm-type :mount {object}} cont-ok cont-no-alloc)
      ))

  (defmethod d 
    (
      (tm tm-empty)
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
      (tm tm-empty)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-no-dealloc (λ()(error 'dealloc-fail)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (declare (ignore tm spill cont-ok cont-no-alloc))
    (funcall cont-no-dealloc)
    )

      
