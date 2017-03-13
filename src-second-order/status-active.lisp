#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Currently these are supported status:

abandoned
parked
empty
active

There is no function on the tm interface that can be called to change the status
of an active machine.  'delete' of the last cell, for example, will result in 
a collision error.  Hence behavior is inherited from the identity transform.

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; status-tm definitions
;;
  (defun-typed park ((tm status-active) &optional ➜)
     (destructuring-bind
       (
         &key
         (➜ok (be t))
         &allow-other-keys
         )
       ➜
       (c◧ (base tm)
         {
           :➜ok (λ()
                  (setf (address tm) 0)
                  (to-parked tm)
                  [➜ok]
                  )
           (o (remove-key-pair ➜ :➜ok))
           })))


;;--------------------------------------------------------------------------------
;; tm-decl-only
;;
  (defun-typed c◧ ((tm status-active) &optional ➜)
     (destructuring-bind
       (
         &key
         (➜ok (be t))
         &allow-other-keys
         )
       ➜
       (c◧ (base tm)
         {
           :➜ok (λ()
                  (setf (address tm) 0)
                  [➜ok]
                  )
           (o (remove-key-pair ➜ :➜ok))
           })))

  (defun-typed s ((tm status-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (s (base tm)
        {:➜ok (λ()
                (incf (address tm))
                [➜ok]
                )
          (o (remove-key-pair ➜ :➜ok))
          })))

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
                 (incf (address-rightmost tm))
                 [➜ok]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })))

  (defun-typed on-leftmost ((tm status-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if (= (address tm) 0) [➜t] [➜∅])
      ))

  (defun-typed on-rightmost ((tm status-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if (= (address-rightmost tm) 0) [➜t] [➜∅])
      ))

;;--------------------------------------------------------------------------------
;; tm-generic
;;
  (defun-typed c◨ ((tm status-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (c◨ (base tm)
        {
          :➜ok (λ()
                 (setf (address tm) (address-rightmost tm))
                 [➜ok]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })))

  (defun-typed as ((tm status-active) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (a (base tm) instance
        {
          :➜ok (λ()
                 (incf (address tm))
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
                 (incf (address tm))
                 (incf (address-rightmost tm))
                 [➜ok]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })))


  (defun-typed d◧ ((tm status-tm) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜collision (λ()(error 'dealloc-collision)))
        &allow-other-keys
        )
      ➜
      (if (= (address-rightmost tm) 0)
        [➜collision]
        (d◧ (base tm) spill
          {
            :➜ok (λ(instance)
                   (decf (address tm))
                   (decf (address-rightmost tm))
                   [➜ok instance]
                   )
            (o (remove-key-pair ➜ :➜ok))
            }))))
        
;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;

  (defun-typed heads-on-same-cell 
    (
      (tm0 status-active)
      (tm1 status-active)
      &optional ➜
      )
    (destructuring-bind
      (
        &key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (entangled tm0 tm1
        {
          :➜t (λ()
                (if (= (address tm0) (address tm1))
                  [➜t]
                  [➜∅]
                  ))
          :➜∅ (λ()
                [➜∅]
                )
          })))
        
 
  (defun-typed heads-on-same-cell
    (
      (tm0 status-active)
      (tm1 tape-machine)
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

  (defun-typed heads-on-same-cell
    (
      (tm0 tape-machine)
      (tm1 status-active)
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))
