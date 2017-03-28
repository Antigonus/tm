#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

functions shared by parked and active

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (defun-typed a◧ ((tm ea-parked-active) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (prins (print "a◧ ea-parked-active"))
      (a◧ (base tm) instance
        {
          :➜ok (λ()
                 (c◧∀* (entanglements tm)
                   (λ(es)
                     (let(
                           (etm (tg:weak-pointer-value (r es)))
                           )
                       (update-tape-after-a◧ (base etm) (base tm))
                       (if
                         (typep etm 'status-parked)
                         (c◧ (base etm))
                         (incf (address etm))
                         )
                       (incf (address-rightmost etm))
                       )))
                 [➜ok]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })))

  (defun-typed d◧ ((tm ea-parked-active) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜collision (λ()(error 'dealloc-collision)))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (prins (print "d◧ ea-parked-active"))
      (labels
        (

          ;;tape originally has only one cell, no active machine is on ◧
          ;;so we transition to empty
          (make-empty () 
            (w (base tm) ∅)
            (c◧∀*
              (entanglements tm) 
              (λ(es) (to-empty (tg:weak-pointer-value (r es))))
              )
            )

          ;;problem: parked machines leave the base head on ◧
          ;;when this is called:
          ;;   -we already checked we are not on rightmost (there is a cell to step to)
          (step-parked-machines () 
            (c◧∀* (entanglements tm)
              (λ(es)
                (let(
                      (etm (tg:weak-pointer-value (r es)))
                      )
                  (when 
                    (typep etm 'status-parked)
                    (s (base etm) {:➜rightmost #'cant-happen})
                    )
                  ))))
          
          (delete-1 () ;presented tape has > one cell, no active machine on ◧
            (d◧ (base tm) ∅
              {:➜ok (λ(instance)
                      (declare (ignore instance))
                      (c◧∀* (entanglements tm)
                        (λ(es)
                          (let(
                                (etm (tg:weak-pointer-value (r es)))
                                )
                          (update-tape-after-d◧ (base etm) (base tm))
                          (when (typep etm 'status-active) (decf (address etm)))
                          (decf (address-rightmost etm))
                          ))))
                :➜collision #'cant-happen ; we already checked address-rightmost = 0
                :➜no-alloc #'cant-happen ; there is no spill
                }))

          (collision (es ct c∅) ;a machine in entanglement group is on ◧ ?
            (let(
                  (etm (tg:weak-pointer-value (r es)))
                  )
              (if 
                (∧
                  (typep etm 'status-active)
                  (= (address etm) 0)
                  )
                [ct]
                [c∅]
                )))
          )

        ;; d◧ function starts here -----
        (c◧∃ (entanglements tm) #'collision
          ➜collision
          (λ()
            (let(
                  (spill-instance (ec◧r (base tm)))
                  )
              (labels(
                       (delete-0 ()
                         (if (= (address-rightmost tm) 0)
                           (make-empty)
                           (progn
                             (step-parked-machines)
                             (delete-1)
                             ))
                         [➜ok spill-instance]
                         )
                       )
                (if spill
                  (as spill spill-instance
                    {
                      :➜ok #'delete-0 
                      (o (remove-key-pair ➜ :➜ok))
                      })
                  (delete-0)
                  )))))
        )))
