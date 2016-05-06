#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

The void projective machine has the control mechanism for a tape, but any attempt to read
or write it is an error.

A tm-void machine takes as an initialization value a type for the tape space it would grow
into should a new cell be allocated.

Calling alloc, #'a, will cause the machine to transition to 'tm-parked.

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

  (defmethod unmount ((tm tm-void))
    (case (HA tm)
      (tm-list ∅)
      (tm-array #())
      (t (error 'can-not-unmount))
      ))

  ;; no need to do anything
  ;; don't know if any compilers will freak with an empty body, so I put t
  (defmethod park ((tm tm-void)) t)


;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod r ((tm tm-void))
    (declare (ignore tm))
    (error 'void-access)
    )
  (defmethod w ((tm tm-void) object)
    (declare (ignore tm object))
    (error 'void-access)
    )

  (defmethod cue-leftmost ((tm tm-void)) t)

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


  (defun a-tm-void
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (let(
          (orig-type (type-of tm)) ; (typep tm tm-void) but might *be* tm-void
          (tm-type (HA tm))
          )
      (change-class tm 'tm-parked)
      (init tm {:tm-type tm-type :mount {object}} 
        cont-ok 
        (λ() ; oops init failed
          (change-class tm orig-type) ; I don't like using change-class twice like this ..
          (funcall cont-no-alloc)
          ))))

  (defmethod a
    (
      (tm tm-void)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (a-tm-void tm object cont-ok cont-no-alloc)
    )

  ;; alloc leftmost is relative to tape leftmost, not the head, so same as #'a in this
  ;; context
  (defmethod a◧
    (
      (tm tm-void)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (a-tm-void tm object cont-ok cont-no-alloc)
    )

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

      
