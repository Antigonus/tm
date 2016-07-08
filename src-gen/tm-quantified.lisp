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
  (defgeneric w* 
    (
      tm
      fill
      &optional
      cont-ok
      cont-rightmost-tm
      cont-parked-tm
      cont-parked-fill
      ))

  (defmethod w* 
    (
      (tm tape-machine)
      fill
      &optional 
      (cont-ok (be t))
      (cont-rightmost-tm (be ∅))
      (cont-parked-tm (λ()(error 'parked-head-use)))
      (cont-parked-fill  (λ()(error 'parked-head-use)))
      )
    (w 
      tm 
      (r fill #'do-nothing (λ()(return-from w* (funcall cont-parked-fill))))
      #'do-nothing
      (λ()(return-from w* (funcall cont-parked-tm)))
      )
    (⟳-loop(λ(cont-loop)
             (s fill
               (λ()(s tm
                     (λ()(w tm cont-loop #'cant-happen))
                     cont-rightmost-tm
                     ))
               cont-ok
               ))))

  (defgeneric s* (tm)
    (:documentation 
      "This is a synonym for cue-to-rightmost. There is no guarantee that intermediate
       cells will be visited."
      ))

  (defmethod s* ((tm tape-machine)) (cue-rightmost tm))

  (defgeneric -s* (tm)
    (:documentation 
      "This is a synonym for cue-to-leftmost. There is no guarantee that intermediate
       cells will be visited."
      ))

  (defmethod -s*((tm tape-machine))(cue-leftmost tm))

  (defgeneric sn (tm n &optional cont-ok cont-rightmost)
    (:documentation 
      "Step n times.  When called, cont-rightmost is passed the current value of n.  For
       example, if the head is on leftmost, and the tape has two cells, and sn is called
       with n set to 3, then the step from rightmost continuation will be called with a
       value of 2.
      "
      ))

  (defmethod sn
    (
      (tm tape-machine)
      (n integer)
      &optional 
      (cont-ok (be t))
      (cont-rightmost (λ(n)(declare (ignore n)) ∅))
      )
    (labels(
             (count-test()
               (when (≤ n 0) (return-from sn (funcall cont-ok)))
               )
             (work()
               (decf n)
               (count-test)
               (take-step) ; step from rightmost test is built into #'s
               )
             (take-step()
               (s 
                 tm 
                 #'work
                 (λ()(return-from sn (funcall cont-rightmost n)))
                 ))
             )
      (count-test)
      (take-step)
      ))

  (defgeneric a* (tm tm-fill &optional cont-ok cont-not-supported cont-no-alloc)
    (:documentation 
      "Calls #'a repeatedly with successive objects from tm-fill. 
       tm will not be stepped.  tm-fill will be stepped.
       The sequence order as it is found on tm-fill will be reversed on tm.
       "      
      ))

  (defgeneric as* (tm tm-fill &optional cont-ok cont-not-supported cont-no-alloc)
    (:documentation 
      "Calls #'as repeatedly with successive objects from tm-fill.
       Both tm and tm-fill will be stepped.
       "
      ))

  ;; cont-no-alloc is not transactional here ... need to fix the other a* versions too
  ;; do we want it to be transactional?  But if it did a spot fix it would have to be
  ;; possible to restart the a* where we left off ..
  (defmethod a*
    (
      (tm tape-machine)
      fill
      &optional
      (cont-ok (be t))
      (cont-not-supported (λ()(error 'not-supported)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (supports-alloc tm
      (λ()
        (⟳-loop
          (λ(cont-loop)
            (a tm (r fill) 
              (λ()(s fill cont-loop cont-ok))
              #'cant-happen
              cont-no-alloc
              ))))
      cont-not-supported
      ))

  (defun as*-1
    (
      tm 
      fill
      &optional
      (cont-ok (be t))
      (cont-not-supported (λ()(error 'not-supported)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (supports-alloc tm
      (λ()
        (⟳-loop
          (λ(cont-loop)
            (as tm (r fill) 
              (λ()(s fill cont-loop cont-ok))
              #'cant-happen
              cont-no-alloc
              ))))
      cont-not-supported
      ))

  (defmethod as*
    (
      (tm0 tape-machine)
      fill
      &optional
      (cont-ok (be t))
      (cont-not-supported (λ()(error 'not-supported)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (as*-1 tm0 fill cont-ok cont-not-supported cont-no-alloc)
    )


;;--------------------------------------------------------------------------------
;; repeated by count operations
;;   more specific versions, if they exist, are surely more efficient
;;
  (defgeneric asn (tm n &optional fill cont-ok cont-rightmost cont-not-supported cont-no-alloc)
    (:documentation 
      "Similar to calling #'as n times. fill provides initialization
       data. tm and fill are both stepped n times."
      ))

  (defmethod asn
    (
      (tm tape-machine)
      (n integer)
      &optional 
      fill
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      (cont-not-supported (λ()(error 'not-supported)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (supports-alloc tm
      (λ()
        (labels(
                 (do-work()
                   (when (≤ n 0) (return-from asn (funcall cont-ok)))
                   (as tm (r fill)
                     (λ()
                       (s fill #'do-work (λ()(funcall cont-rightmost n)))
                       )
                     #'cant-happen
                     cont-no-alloc
                     )
                   (decf n)
                   )
                 )
          (do-work)
          ))
      cont-not-supported
      ))



