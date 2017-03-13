#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; tm-decl-only
;;
  (defun-typed a ((tm ea-active) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (a (base tm) instance
        {
          :➜ok (λ()
                 (c◧∀* (instances (entanglements tm))
                   (λ(es)
                     (let(
                           (entangled-tm (r (tg:weak-pointer-value (r es))))
                           )
                       (incf (address-rightmost entangled-tm))
                       (when
                         (> (address entangled-tm) (address tm))
                         (incf (address entangled-tm))
                         ))))
                 [➜ok]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })))


;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (defun-typed a◧ ((tm ea-active) instance &optional ➜)
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
                     (let(
                           (entangled-tm (tg:weak-pointer-value (r es)))
                           )
                       (incf (address-rightmost entangled-tm))
                       (incf (address entangled-tm))
                       )))
                 [➜ok]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })))

  (defun-typed d ((tm ea-active) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜collision (λ()(error 'dealloc-collision)))
        &allow-other-keys
        )
      ➜
      (labels(
               (d-no-collision ()
                 (d (base tm) spill
                   {
                     :➜ok (λ(instance)
                            (c◧∀* (instances (entanglements tm))
                              (λ(es)
                                (let(
                                      (entangled-tm (tg:weak-pointer-value (r es)))
                                      )
                                  (decf (address-rightmost entangled-tm))
                                  (when
                                    (> (address entangled-tm) (address tm))
                                    (decf (address entangled-tm))
                                    ))))
                            [➜ok instance]
                            )
                     :➜collision #'cant-happen
                     (o (remove-key-pairs ➜ {:➜ok :➜collision}))
                     }))
               )
        (c◧∃ (instances (entanglements tm))
          (λ(instances ct c∅)
            (let(
                  (entangled-tm (tg:weak-pointer-value (r instances)))
                  )
              (if
                (∧
                  (≠ (address entangled-tm) 0)
                  (= (address tm) (- (address entangled-tm) 1))
                  )
                [ct]
                [c∅]
                )))
          ➜collision
          #'d-no-collision
          ))))

  (defun-typed d◧ ((tm ea-active) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜collision (λ()(error 'dealloc-collision)))
        &allow-other-keys
        )
      ➜
      (labels(
               (d◧-no-collision ()
                 (d◧ (base tm) spill
                   {
                     :➜ok (λ(instance)
                            (c◧∀* (instances (entanglements tm))
                              (λ(es)
                                (let(
                                      (entangled-tm (tg:weak-pointer-value (r es)))
                                      )
                                  (decf (address-rightmost entangled-tm))
                                  (decf (address entangled-tm))
                                  )))
                            [➜ok instance]
                            )
                     :➜collision #'cant-happen
                     (o (remove-key-pairs ➜ {:➜ok :➜collision}))
                     }))
               )
        (c◧∃ (instances (entanglements tm))
          (λ(es ct c∅)
            (if
              (= (address (tg:weak-pointer-value (r es))) 0)
              [ct]
              [c∅]
              ))
          ➜collision
          #'d◧-no-collision
          ))))
                               
;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
