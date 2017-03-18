#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; unique to ea
;;
  ;; code would be simpler if entanglements had a circular tape
  (defun clean-entanglements (tm)
    (let(
          (es (entanglements tm))
          )
      (c◧∀* es
        (λ(es)
          (when
            (∧
              (¬ (on-rightmost es))
              (¬ (tg:weak-pointer-value (esr es)))
              )
            (d es)
            )))
      (c◧ es)
      (when 
        (∧
          (¬ (tape-length-is-one es))
          (¬ (tg:weak-pointer-value (r es)))
          )
        (d◧ es)
        ))
    t
    )

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
          (make-empty () ;tape originally has only one cell, no active machine on ◧
            (w (base tm) ∅)
            (c◧∀* (entanglements tm) (λ(es) (to-empty (r es))))
            )
          (step-parked-machines () ;problem: parked machines leave the base head on ◧
            (c◧∀* (entanglements tm)
              (λ(es)
                (if (typep (r es) 'status-parked) (s (base tm)))
                )))
          (delete-leftmost () ;tape originally > one cell, no active machine on ◧
            (d◧ (base tm) spill
              {:➜ok (λ(instance)
                      (declare (ignore instance))
                      (c◧∀* (entanglements tm)
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

        (c◧∃ (entanglements tm) #'collision
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
                       [➜ok]
                       )
                (o (remove-key-pair ➜ :➜ok))
                })))
        )))


