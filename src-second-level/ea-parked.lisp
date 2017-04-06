#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; entanglements support
;;
  (defun-typed entangled-on-same-cell ((tm ea-parked) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

  (defun-typed entangled-on-right-neighbor-cell ((tm ea-parked) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (entangled-on-leftmost (entanglements tm) ➜t ➜∅)
      ))
      


;;--------------------------------------------------------------------------------
;; tm-decl-only
;;
  (defun-typed c◧ ((tm ea-parked) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (to-active tm)
      ;; address is already 0, and (base tm) already on leftmost -- by convention
      [➜ok]
      ))


;;--------------------------------------------------------------------------------
;;tm-generic
;;
  (defun-typed c◨ ((tm ea-parked) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (call-next-method tm
        {
          :ok (λ()
                (setf (address tm) (address-rightmost tm))
                [➜ok]
                )
          (o (remove-key-pair ➜ :➜ok))
          }
        )))

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  ;; see ea-parked-active for a◧, d◧


                    
