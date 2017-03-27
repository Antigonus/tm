#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

functions shared by parked and active

|#

(in-package #:tm)
;;

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (defun-typed a◧ ((tm status-parked-active) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (a◧ (base tm) instance
        {
          :➜ok (λ()
                 (incf (address tm))
                 (incf (address-rightmost tm))
                 [➜ok]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })))
