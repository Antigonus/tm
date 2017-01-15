#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Quantified operations.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; repeated until end of tape operations
;;   more specific versions, if they exist, are surely more efficient
;;
  ;; if you want fill to be a single value, make it a singular-affine machine
  ;; cont-rightmost is called when the fill machine hits rightmost
  (def-function-class w* 
    (
      tm
      fill
      &optional
      cont-ok
      cont-rightmost-tm
      &rest ⋯
      )
    (:documentation
      "Reads fill, and writes tm, steps both, and repeats until fill passes rightmost;
       it then follows cont-ok.  Should tm hit rightmost first, then cont-rightmost-tm.
       "
      ))

  (defun-typed w* 
    (
      (tm tape-machine)
      fill
      &optional 
      (cont-ok (be t))
      (cont-rightmost-tm (be ∅))
      &rest ⋯
      )
    (declare (ignore ⋯))
    (w tm (r fill))
    (⟳(λ(again)
        (s fill
          (λ()(s tm
                (λ()(w tm (r fill))[again])
                cont-rightmost-tm
                ))
          cont-ok
          ))))

  (def-function-class s* (tm)
    (:documentation 
      "This is a synonym for cue-to-rightmost. There is no guarantee that intermediate
       cells will be visited."
      ))

  (defun-typed s* ((tm tape-machine)) (cue-rightmost tm))

  (def-function-class -s* (tm)
    (:documentation 
      "This is a synonym for cue-to-leftmost. There is no guarantee that intermediate
       cells will be visited."
      ))

  (defun-typed -s*((tm tape-machine))(cue-leftmost tm))

  ;; note the fill data will be reversed at the tm insert point
  ;; use as* to fill without reversal
  ;; use eas* to fill forward without moving tm (requires at least nd-tm)
  (def-function-class a* (tm fill &optional cont-ok cont-no-alloc)
    (:documentation 
      "Calls #'a repeatedly with successive instances from tm-fill. 
       tm will not be stepped.  tm-fill will be stepped.
       The sequence order as it is found on tm-fill will be reversed on tm.
       "      
      ))

  ;; cont-no-alloc is not transactional here ... need to fix the other a* versions too
  ;; do we want it to be transactional?  But if it did a spot fix it would have to be
  ;; possible to restart the a* where we left off ..
  (defun-typed a*
    (
      (tm tape-machine)
      fill
      &optional
      (cont-ok (be t))
      (cont-no-alloc #'alloc-fail)
      )
    (⟳(λ(again)
        (a tm (r fill) 
          (λ()(s fill again cont-ok))
          cont-no-alloc
          ))))

  (def-function-class as* (tm fill &optional cont-ok cont-no-alloc)
    (:documentation 
      "Calls #'as repeatedly with successive instances from tm-fill.
       Both tm and tm-fill will be stepped.
       "
      ))


  (defun as*-1
    (
      tm 
      fill
      &optional
      (cont-ok (be t))
      (cont-no-alloc #'alloc-fail)
      )
    (⟳(λ(again)
        (as tm (r fill) 
          (λ()(s fill again cont-ok))
          cont-no-alloc
          ))))

  (defun-typed as*
    (
      (tm0 tape-machine)
      fill
      &optional
      (cont-ok (be t))
      (cont-no-alloc #'alloc-fail)
      )
    (as*-1 tm0 fill cont-ok cont-no-alloc)
    )


;;--------------------------------------------------------------------------------
;; repeated by count operations
;;   more specific versions, if they exist, are surely more efficient
;;
  (def-function-class sn (tm n &optional cont-ok cont-rightmost)
    (:documentation 
      "Step n times.  When called, cont-rightmost is passed the current value of n.  For
       example, if the head is on leftmost, and the tape has two cells, and sn is called
       with n set to 3, then the step from rightmost continuation will be called with a
       value of 2.
      "
      ))

  (defun-typed sn
    (
      (tm tape-machine)
      (n integer)
      &optional 
      (cont-ok (be t))
      (cont-rightmost (λ(n)(declare (ignore n)) ∅))
      )
    (⟳(λ(again)
        (if
          (> n 0)
          (s tm
            (λ()(decf n)[again])
            (λ()[cont-rightmost n])
            )
          [cont-ok]
          ))))


  (def-function-class asn (tm n &optional fill cont-ok cont-rightmost-fill cont-no-alloc)
    (:documentation 
      "Similar to calling #'as n times. fill provides initialization
       data. tm and fill are both stepped n times."
      ))

  ;; interesting that the fill iterator is one ahead, pointing at the next
  ;; thing to be written.  Thus this routine does not maintain inclusive bounds.
  (defun-typed asn
    (
      (tm tape-machine)
      (n integer)
      &optional 
      fill
      (cont-ok (be t))
      (cont-rightmost-fill (λ(cnt)(declare (ignore cnt))∅))
      (cont-no-alloc (λ(tm n)(declare (ignore tm n))(error 'alloc-fail)))
      )
    (⟳(λ(again)
        (if
          (> n 0)
          (as tm (r fill)
            (λ()
              (s fill 
                (λ()(decf n) [again])
                (λ()[cont-rightmost-fill n])
                ))
            cont-no-alloc
            )
          [cont-ok]
          ))))



