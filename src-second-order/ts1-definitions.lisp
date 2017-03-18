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
  (defun-typed a◧ ((tm ea-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (let(have-lock)
        (bt:acquire-lock lock)
        (setf have-lock t)
        (unwind-protect
          (a◧ (base tm) instance
            {
              :➜ok (λ()
                     (c◧∀* (entanglements (locked-entanglements tm))
                       (λ(es)
                         (update-tape-after-a◧ (base (r es)) (base tm))
                         (incf (address (r es)))
                         (incf (address-rightmost (r es)))
                         ))
                     (bt:release-lock lock)
                     (setf have-lock ∅)
                     [➜ok]
                     )
              :no-alloc (λ()
                          (bt:release-lock lock)
                          (setf have-lock ∅)
                          [➜no-alloc]
                          )
              })
          (when have-lock (bt:release-lock))
          ))))

  (defun-typed d◧ ((tm ea-tm) &optional spill ➜)
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
            (c◧∀* (entanglements (locked-entanglements tm)) (λ(es) (to-empty (r es))))
            )
          (step-parked-machines () ;problem: parked machines leave the base head on ◧
            (c◧∀* (entanglements (locked-entanglements tm))
              (λ(es)
                (if (typep (r es) 'status-parked) (s (base tm)))
                )))
          (delete-leftmost () ;tape originally > one cell, no active machine on ◧
            (d◧ (base tm) spill
              {:➜ok (λ(instance)
                      (declare (ignore instance))
                      (c◧∀* (locked-entanglements tm)
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
                (= (address (r es)) 0)
                )
              [ct]
              [c∅]
              ))
          )

        (let(
              (lock (lock (locked-entanglements tm)))
              (entanglements (entanglements (locked-entanglements tm)))
              have-lock ; after we release, others might set the lock, so we need our own flag
              )
          (bt:acquire-lock lock t)
          (setf have-lock t)
          (unwind-protect ; prevents orphaning the lock due to unexpected excdeptions
            (c◧∃ entanglements #'collision
              ➜collision
              (λ()
                (a spill (r (base tm))
                  {
                    :➜ok (λ()
                           (if (= (address-rightmost tm) 0)
                             (make-empty)
                             (progn
                               (step-parked-machines)
                               (delete-leftmost)
                               ))
                           (bt:release-lock lock)
                           (setf have-lock ∅)
                           [➜ok]
                           )
                    :➜no-alloc (λ() 
                                 (bt:release-lock lock)
                                 (setf have-lock ∅)
                                 [➜no-alloc]
                                 )
                    })))
            (when have-lock (bt:release-lock lock))
            )))))

