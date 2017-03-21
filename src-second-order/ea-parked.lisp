#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|#

(in-package #:tm)

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
  ;; see ea-definitions for a◧, d◧


                    
