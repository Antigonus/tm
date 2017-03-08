#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Manager synchronizes operation on tape machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-type ea-tm (status-tm)
    (
      (entanglements ; a solo tm of machines in this entanglement group
        :initarg :entanglements
        :accessor entanglements
        )
      ))

  (def-type ea-abandoned (ea-tm status-abandoned)())
  (def-type ea-active    (ea-tm status-active)())
  (def-type ea-empty     (ea-tm status-empty)())
  (def-type ea-parked    (ea-tm status-parked)())

;;--------------------------------------------------------------------------------
;; state transition functions
;;
  (defun-typed to-abandoned ((tm ea-tm)) (change-class tm 'ea-abandoned))
  (defun-typed to-active    ((tm ea-tm)) (change-class tm 'ea-active))
  (defun-typed to-empty     ((tm ea-tm)) (change-class tm 'ea-empty))
  (defun-typed to-parked    ((tm ea-tm)) (change-class tm 'ea-parked))

;;--------------------------------------------------------------------------------
;;
  (defun-typed init
    (
      (tm ea-tm)
      (init-parms cons)
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (call-next-method tm init-parms
        {
          :➜ok (λ(instance)
                 (mk 'list-solo-tm {:tape {tm}}
                   {
                     :➜ok (λ(a-solo-tm)
                            (setf (entanglements tm) a-solo-tm)
                            [➜ok instance]
                            )
                     :➜fail #'cant-happen
                     :➜no-alloc ➜no-alloc
                     }))
          (o (remove-key-pair ➜ :➜ok))
          })
      ))

   
;;--------------------------------------------------------------------------------
;; copy
;;
  (defun-typed entangle ((tm-orig ea-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        ;; (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜  
      (let(
            (i (make-instance (type-of tm-orig)))
            )
        (a◧ (entanglements tm-orig) i
          {
            :➜ok (λ()
                   (setf (base i) (entangle (base tm-orig)))
                   (setf (entanglements i) (entanglements tm-orig))
                   (setf (address i) (address tm-orig))
                   (setf (address-rightmost i) (address-rightmost tm-orig))
                   [➜ok i]
                   )
            (o (remove-key-pair ➜ :➜ok))
            }))))

