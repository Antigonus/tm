#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Manager synchronizes operation on tape machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-type ts1-tm (ea-tm)
    (
      ))

  (def-type ts1-abandoned (ts1-tm ea-abandoned)())
  (def-type ts1-active    (ts1-tm ea-active)())
  (def-type ts1-empty     (ts1-tm ea-empty)())
  (def-type ts1-parked    (ts1-tm ea-parked)())

;;--------------------------------------------------------------------------------
;; state transition functions
;;
  (defun-typed to-abandoned ((tm ts1-tm)) (change-class tm 'ts1-abandoned))
  (defun-typed to-active    ((tm ts1-tm)) (change-class tm 'ts1-active))
  (defun-typed to-empty     ((tm ts1-tm)) (change-class tm 'ts1-empty))
  (defun-typed to-parked    ((tm ts1-tm)) (change-class tm 'ts1-parked))

;;--------------------------------------------------------------------------------
;;
  (defun-typed init
    (
      (tm ts1-tm)
      (init-parms cons)
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (call-next-method tm init-parms
        {
          :➜ok (λ(instance)
                 (setf (entanglements-lock instance) (bt:make-lock))
                 [➜ok instance]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })
      ))

   
;;--------------------------------------------------------------------------------
;; copy
;;
  (defun-typed entangle ((tm-orig ts1-tm) &optional ➜)
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

