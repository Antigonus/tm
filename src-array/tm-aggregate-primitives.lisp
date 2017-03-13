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
  (defun-typed r ((tm tm-smooth))
    (r (r (tape tm)))
    )

  (defun-typed w ((tm tm-aggregate) instance)
    (if 
      (typep instance 'tape-machine)
      (call-next-method tm instance)
      (error 'instance-not-tape-machine)
      ))

  (defun-typed w ((tm tm-smooth) instance)
    (w (r (tape tm)))
    t
    )

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defun-typed c◧  ((tm tm-smooth)) 
    (c◧ (tape tm))
    (c◧ (r (tape tm)))
    )

;;--------------------------------------------------------------------------------
;;  head location predicates
;;
  (defun-typed heads-on-same-cell 
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
  (defun-typed s
    (
      (tm tm-smooth)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (s (r (tape tm))
      cont-ok
      (λ() (s (tape tm)
             (λ() (c◧ (r (tape tm))) (funcall cont-ok))
             cont-rightmost
             ))))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defun-typed a 
    (
      (tm tm-aggregate)
      instance 
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    (if 
      (typep instance 'tape-machine)
      (call-next-method tm instance cont-ok cont-no-alloc)
      (error 'instance-not-tape-machine)
      ))

  (defun-typed a 
    (
      (tm tm-smooth)
      instance 
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )
    (declare (ignore tm instance cont-ok))
    (funcall cont-no-alloc)
    )

;;--------------------------------------------------------------------------------
;; deallocating cells
;;
  (defun-typed d 
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

    
