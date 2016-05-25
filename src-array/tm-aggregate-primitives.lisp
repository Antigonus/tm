#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

see notes in tm-smooth header

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defmethod r ((tm tm-smooth))
    (r (r (tape tm)))
    )

  (defmethod w ((tm tm-aggregate) object)
    (if 
      (typep object 'tape-machine)
      (call-next-method tm object)
      (error 'object-not-tape-machine)
      ))

  (defmethod w ((tm tm-smooth) object)
    (w (r (tape tm)))
    t
    )

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defmethod cue-leftmost  ((tm tm-smooth)) 
    (cue-leftmost (tape tm))
    (cue-leftmost (r (tape tm)))
    )

;;--------------------------------------------------------------------------------
;;  head location predicates
;;
  (defmethod heads-on-same-cell 
    (
      (tm0 tm-smooth) 
      (tm1 tm-smooth) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell (r tm0) (r tm1) cont-true cont-false)
    )

;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defmethod s
    (
      (tm tm-smooth)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (s (r (tape tm))
      cont-ok
      (λ() (s (tape tm)
             (λ() (cue-leftmost (r (tape tm))) (funcall cont-ok))
             cont-rightmost
             ))))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defmethod a 
    (
      (tm tm-aggregate)
      object 
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    (if 
      (typep object 'tape-machine)
      (call-next-method tm object cont-ok cont-no-alloc)
      (error 'object-not-tape-machine)
      ))

  (defmethod a 
    (
      (tm tm-smooth)
      object 
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    (declare (ignore tm object cont-ok))
    (funcall cont-no-alloc)
    )

;;--------------------------------------------------------------------------------
;; deallocating cells
;;
  (defmethod d 
    (
      (tm tm-smooth)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-no-dealloc (λ()(error 'tm-deallocation-request-at-rightmost)))
      (cont-no-alloc (λ()(error 'tm-alloc-fail :text "can not spill")))
      )
    (declare (ignore tm spill cont-ok cont-no-alloc))
    (funcall cont-no-dealloc)
    )

    
