#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  We defined a transform as a function that would take one object as input, and then
  produce one object as output.  I did not call that a one to one mapping, because, should
  the transform function have state, it may return a different output object at different
  times though given same input object.  It is always one object in, and one object out,
  but it might not be one to one. It might be one to many. Any example of such a transform
  is a moving average filter.

  Transform functions have some drawbacks. For example, they can not be used to drop a
  value on read.  When read is called, it must produce an object. When write is given an
  object, we might fail to do the write, but even then the old value is still there, so in
  a sense, this becomes the tranform output. Similarly, we can not read or write multiple
  values, because #r and #w do not have the continutations to handle end cases for
  stepping or allocation.

  Here we define a morph function to play a similar role to the transform function, but to
  also facilitate multiple value return.  The morph function accepts a single object as
  input, but then produces zero or more outputs.

  The two primary functions that can support mophing are #'a and #'d, and their end case
  variations.  #'a and #'d are both writing functions.  They both allocate cells at their
  destination.  The differences is that #'d deallocates the cell from the source after
  reading, and attempts to re-attach it to the spill tape. If this is not possible then
  #'d is allowed to move the object instead of the cell, and then to drop the cell. So we
  will make a simplification, and drop the cell and give the object to the morph function.
  Hence, we can use the same morph function with both #'a and #'d.

  The reason this is morphological rather than transformational, is that the structure of
  the output tape is changed, and possibly the input tape as well.  I.e. we allocate and
  deallocate cells from tapes. Transformation doesn't mess with allocation.

|#


(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-morph (tape-machine)())

  (defstruct morph
    morph
    tm
    )

  (defun mk-tm-morph
    (
      &optional
      init
      (cont-ok #'echo) 
      (cont-fail (λ() (error 'tm-mk-bad-init-type :text "expected a morph struct")))
      )
    (let(
          (tm (make-instance 'tm-transform))
          )
      (unless
        (∧
          init
          (typep init 'morph)
          )
        (funcall cont-fail)
        )
      (setf (tape tm) init)
      (funcall cont-ok tm)
      ))

  (mk-tm-hook 'tm-morph #'mk-tm-morph)

;;--------------------------------------------------------------------------------
;; essential methods
;;
  (defmethod r ((tm tm-morph)) 
    (r (morph-tm (tape tm)))
    )

  (defmethod w ((tm tm-morph) object)
    (w (morph-tm (tape tm)) object)
    )
 
  (defmethod cue-leftmost  ((tm tm-morph)) 
    (cue-leftmost (morph-tm (tape tm)))
    )

  (defun heads-on-same-cell-morph-0 (tm0 tm1 cont-true cont-false)
    (heads-on-same-cell-morph-0 
      (morph-tm (tape tm0))
      (morph-tm (tape tm1))
      cont-true
      cont-false
      ))

  (defmethod heads-on-same-cell 
    (
      (tm0 tm-morph) 
      (tm1 tape-machine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-morph-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod heads-on-same-cell 
    (
      (tm0 tape-machine) 
      (tm1 tm-morph) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell-morph-0 tm0 tm1 cont-true cont-false)
    )

  (defmethod s
    (
      (tm tm-morph)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (s (morph-tm (tape tm)) cont-ok cont-rightmost)
    )

  (defmethod a
    (
      (tm tm-morph)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    (a (morph-tm (tape tm)) object cont-ok cont-no-alloc)
    )

  (defmethod as
    (
      (tm tm-morph)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    (let(
          (result (multiple-value-list (funcall (morph-morph tm) object)))
          )
      (when
        result
        (let(
              (tm-result (mk-tm 'list result))
              (tm-morph (morph-tm (tape tm)))
              )
          (⟳ tm-result 
            (λ()
              (a 
                tm-morph
                (r tm-result) 
                #'do-nothing
                (λ()(return-from as (funcall cont-no-alloc)))
                ))
            cont-ok
            )))
      ))

  ;; morph not yet implemented on d d◧.
  (defmethod d 
    (
      (tm tm-morph)
      &optional 
      spill
      (cont-ok (be t))
      (cont-rightmost (λ()(error 'tm-deallocation-request-at-rightmost)))
      (cont-no-alloc 
        (λ()(error 'tm-alloc-fail :text "could not spill")))
      )
    (d (morph-tm (tape tm)) spill cont-ok cont-rightmost cont-no-alloc)
    )

