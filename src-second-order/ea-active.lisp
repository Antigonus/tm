#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; tm-decl-only
;;
  (defun inc-rightside-addresses (tm)
    (bt:with-lock-held ((lock (locked-entanglements tm)))
      (c◧∀* (entanglements (locked-entanglements tm))
        (λ(es)
          (let(
                (entangled-tm (tg:weak-pointer-value (r es)))
                )
            (when entangled-tm
              (incf (address-rightmost entangled-tm))
              (when
                (> (address entangled-tm) (address tm))
                (incf (address entangled-tm))
                )))))))

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
                 (inc-rightside-addresses tm)
                 [➜ok]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })))


;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  ;; see ea-definitions for a◧ and d◧

  (defun dec-rightside-addresses (tm)
    (bt:with-lock-held ((lock (locked-entanglements tm)))
      (c◧∀* (entanglements (locked-entanglements tm))
        (λ(es)
          (let(
                (entangled-tm (tg:weak-pointer-value (r es)))
                )
            (when entangled-tm
              (decf (address-rightmost entangled-tm))
              (when (> (address entangled-tm) (address tm))
                (decf (address entangled-tm))
                )))))))

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
                            (dec-rightside-addresses tm)
                            [➜ok instance]
                            )
                     :➜collision #'cant-happen
                     (o (remove-key-pairs ➜ {:➜ok :➜collision}))
                     }))
               )
        (if
          (bt:with-lock-held ((lock (locked-entanglements tm)))
            (c◧∃ (entanglements (locked-entanglements tm))
              (λ(entanglements ct c∅)
                (let(
                      (entangled-tm (tg:weak-pointer-value (r entanglements)))
                      )
                  (if
                    (∧
                      entangled-tm
                      (≠ (address entangled-tm) 0)
                      (= (address tm) (- (address entangled-tm) 1))
                      )
                    [ct]
                    [c∅]
                    )))))
            [➜collision]
            (d-no-collision)
            ))))

                               
;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
