#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
;; more specific versions can be found for status-abandoned and status-empty,
;; so these will only apply to status-parked and status-active
;;
  (defun-typed a◧ ((tm status-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (a◧ (base tm) instance
        {
          :➜ok (λ()
                 (cue-leftmost (entanglements tm))
                 (∀* (entanglements tm)
                   (λ(es)
                     (update-tape-after-a◧ (base (r es)) (base tm))
                     (incf (address (r es)))
                     (incf (address-rightmost (r es)))
                     ))
                 [➜ok]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })))

  (defun-typed d◧ ((tm status-tm) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜collision (λ()(error 'dealloc-collision)))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (labels
        (
          (make-empty () ;tape originally has only one cell, no active machine on ◧
            (w (base tm) ∅)
            (cue-leftmost (entanglements tm))
            (∀* (entanglements tm) (λ(es) (change-class (re es) 'status-empty)))
            )
          (step-parked-machines () ;problem: parked machines leave the base head on ◧
            (cue-leftmost (entanglements tm))
            (∀* (entanglements tm)
              (λ(es)
                (if (typep (r es) 'status-parked) (s (base tm)))
                )))
          (delete-leftmost () ;tape originally > one cell, no active machine on ◧
            (d◧ (base tm) spill
              {:➜ok (λ(instance)
                      (declare (ignore instance))
                      (cue-leftmost (entanglements tm))
                      (∀* (entanglements tm)
                        (λ(es)
                          (update-tape-after-d◧ (base (r es)) (base tm))
                          (when (typep (r es) 'status-active) (decf (address (r es))))
                          (decf (address-rightmost (r es)))
                          )))
                :➜collision #'cant-happen
                :➜no-alloc ➜no-alloc
                }))
          (collision (es ct c∅) ;a machine in entanglement group is on ◧ ?
            (if 
              (∧
                (typep (r es) 'status-active)
                (= (address (r es) 0))
                )
              [ct]
              [c∅]
              ))
          )

        (cue-leftmost (entanglements tm))
        (∃ (entanglements tm) #'collision
          {
            :➜t ➜collision
            :➜∅ (λ()
                  (a spill (r (base tm))
                    {
                      :➜ok (λ()
                             (if (= (address-rightmost tm) 0)
                               (make-empty)
                               (progn
                                 (step-parked-machines)
                                 (delete-leftmost)
                                 ))
                             [➜ok]
                             )
                      (o (remove-key-pair ➜ :➜ok))
                      }))})

