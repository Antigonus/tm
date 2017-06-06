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

  (defun-typed epa ((tm ea-parked-active) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (labels(
               (fix-tapes-inc-addresses ()
                 (-s*∀* (entanglements tm)
                   (λ(es)
                     (let(
                           (etm (r es))
                           )
                       (when etm
                         (update-tape-after-epa (base etm) (base tm))
                         (if
                           (typep etm 'parked)
                           (-s* (base etm))
                           (incf (address etm))
                           )
                         (incf (address-rightmost etm))
                         )))))
              )
      ;; (prins (print "epa ea-parked-active"))
      (epa (base tm) instance
        {
          :➜ok (λ()
                 (fix-tapes-inc-addresses)
                 [➜ok]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })
        )))

  (defun-typed epd ((tm ea-parked-active) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜collision (λ()(error 'dealloc-collision)))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      ;; (prins (print "epd ea-parked-active"))
      (labels
        (

          ;;tape originally has only one cell, no active machine is on -s*
          ;;so we transition to empty
          (make-empty () 
            (w (base tm) ∅)
            (-s*∀* (entanglements tm) 
              (λ(es)
                (let(
                      (etm (r es))
                      )
                  (when etm (to-empty etm))
                  ))))

          ;;problem: parked machines leave the base head on -s*
          ;;when this is called:
          ;;   -we already checked we are not on rightmost (there is a cell to step to)
          (step-parked-machines () 
            (-s*∀* (entanglements tm)
              (λ(es)
                (let(
                      (etm (r es))
                      )
                  (when etm
                    (when (typep etm 'parked)
                      (s (base etm) {:➜rightmost #'cant-happen})
                      ))))))
          
          (fix-tapes-dec-addresses ()
            (-s*∀* (entanglements tm)
              (λ(es)
                (let(
                      (etm (r es))
                      )
                  (when etm
                    (update-tape-after-epd (base etm) (base tm))
                    (when (typep etm 'active) (decf (address etm)))
                    (decf (address-rightmost etm))
                    )))))
          )

        ;; epd function starts here -----
        (entangled-on-leftmost (entanglements tm)
          ➜collision
          (λ()
            (let(
                  (spill-instance (e-s*r (base tm)))
                  )
              (labels(
                       (delete-0 ()
                         (if (= (address-rightmost tm) 0)
                           (make-empty)
                           (progn
                             (step-parked-machines)
                             (epd (base tm) ∅
                               {:➜ok (λ(instance)
                                       (declare (ignore instance))
                                       (fix-tapes-dec-addresses)
                                       )
                                 :➜collision #'cant-happen ; we already checked address-rightmost = 0
                                 :➜no-alloc #'cant-happen ; there is no spill
                                 })))
                         [➜ok spill-instance]
                         )
                       )
                (if spill
                  (as spill spill-instance
                    {
                      :➜ok #'delete-0 
                      :➜no-alloc ➜no-alloc
                      })
                  (delete-0)
                  )
                ))))
        )))
