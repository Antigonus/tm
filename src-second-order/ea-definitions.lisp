#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; unique to ea
;;
  (def-function-class clean-entanglements (tm))

  ;; machines are considered to be entangled with themselves, thus
  ;; we can never have an empty entanglements machine
  (defun-typed clean-entanglements ((tm ea-tm))
    (let(
          (es (entanglements tm))
          )

      (c◧ es)
      (⟳
        (λ(➜again)
          (if
            (on-rightmost es)
            (return-from clean-entanglements t)
            (when (tg:weak-pointer-value (r es))
              (when (on-leftmost es) (s es {:➜rightmost #'cant-happen}))
              (d◧ es)
              [➜again]
              ))))
      (⟳
        (λ(➜again)
          (if
            (on-rightmost es)
            (return-from clean-entanglements t)
            (if
              (tg:weak-pointer-value (esr es))
              (d es)
              (s es {:➜ok ➜again :➜rightmost (λ()(return-from clean-entanglements t))})
              ))))

      [#'cant-happen]
      ))


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
        &allow-other-keys
        )
      ➜
      (a◧ (base tm) instance
        {
          :➜ok (λ()
                 (c◧∀* (entanglements tm)
                   (λ(es)
                     (update-tape-after-a◧ (base (r es)) (base tm))
                     (incf (address (r es)))
                     (incf (address-rightmost (r es)))
                     ))
                 [➜ok]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })))

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
          
          (delete-1 () ;tape originally > one cell, no active machine on ◧
            (d◧ (base tm) spill
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
                :➜collision #'cant-happen
                :➜no-alloc ➜no-alloc
                }))

          (delete-0 () ;tape originally > one cell, no active machine on ◧
            (if (= (address-rightmost tm) 0)
              (make-empty)
              (progn
                (step-parked-machines)
                (delete-1)
                )))


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

        (c◧∃ (entanglements tm) #'collision
          ➜collision
          (λ()
            (let(
                  (spill-instance (ec◧r (base tm)))
                  )
              (if spill
                (a spill spill-instance
                  {
                    :➜ok (λ()
                           (delete-0)
                           [➜ok spill-instance]
                           )
                    (o (remove-key-pair ➜ :➜ok))
                    })
                (progn
                  (delete-0)
                  [➜ok spill-instance]
                  )))))
        )))


