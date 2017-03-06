#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; tm-decl-only
;;
  (defun-typed a ((tm status-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (a (base tm) instance
        {
          :➜ok (λ()
                 (cue-leftmost (entanglements tm))
                 (∀* (entanglements tm)
                   (λ(es)
                     (incf (address-rightmost (r es)))
                     (when
                       (> (address (r es)) (address tm))
                       (incf (address (r es)))
                       )))
                 [➜ok]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })))


;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
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
                     (incf (address-rightmost (r es)))
                     (incf (address (r es)))
                     ))
                 [➜ok]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })))

  (defun-typed d ((tm status-tm) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (if (= (address-rightmost tm) 0)
        [➜collision]
        (d◧ (base tm) spill
          {
            :➜ok (λ(instance)
                   (cue-leftmost (entanglements tm))
                   (∀* (entanglements tm)
                     (λ(es)
                       (decf (address-rightmost (r es)))
                       (when
                         (> (address (r es)) (address tm))
                         (decf (address (r es)))
                         )))
                   [➜ok instance]
                   )
            (o (remove-key-pair ➜ :➜ok))
            }))))

  (defun-typed d◧ ((tm status-tm) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (if (= (address-rightmost tm) 0)
        [➜collision]
        (d◧ (base tm) spill
          {
            :➜ok (λ(instance)
                   (cue-leftmost (entanglements tm))
                   (∀* (entanglements tm)
                     (λ(es)
                       (decf (address-rightmost (r es)))
                       (decf (address (r es)))
                       ))
                   [➜ok instance]
                   )
            (o (remove-key-pair ➜ :➜ok))
            }))))

;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;