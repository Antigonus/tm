#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  There is a synchronizaiton problem when machines are entangled.  When we have
  a set of entangled machines, when any member of that set becomes empty, all members
  of the set are empty.  Before deleting a cell from the tape while using one machine,
  we must check that no other machine has a head on that cell.  etc.

  Each entanglement set has a corresponding machine of type 'entanglements'.  An
  entanglements machine holds instances of type 'weak pointer to an entangled machine'.
  When an entangled machine goes out of scope and is garbage collected, its weak pointer
  value becomes ∅.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-type ea-tm (status-tm)
    (
      (entanglements
        :initarg :entanglements
        :accessor entanglements
        )
      ))

  (def-type ea-parked-active (ea-tm)())

  (def-type ea-abandoned (status-abandoned ea-tm)())
  (def-type ea-active    (ea-parked-active status-active ea-tm)())
  (def-type ea-empty     (status-empty ea-tm)())
  (def-type ea-parked    (ea-parked-active status-parked ea-tm)())


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
                 (mk 'list-solo-tm {:tape {(tg:make-weak-pointer tm)}}
                   {
                     :➜ok (λ(entanglements)
                            (setf (entanglements tm) entanglements)
                            [➜ok instance]
                            )
                     :➜fail #'cant-happen
                     :➜no-alloc ➜no-alloc
                     }))
          (o (remove-key-pair ➜ :➜ok))
          })))
   
;;--------------------------------------------------------------------------------
;; copy
;;

  ;; accepts an instance (typep 'ea-tm), returns an entangled instance of the same type
  ;;   .. probably should have 'call-next-method' for some of the slots
  ;;
    (defun-typed entangle ((tm-orig ea-tm) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          (➜no-alloc #'alloc-fail)
          &allow-other-keys
          )
        ➜  
        (let*(
               (tm-entangled         (make-instance (type-of tm-orig)))
               (pt-tm-entangled      (tg:make-weak-pointer tm-entangled))
               (entanglements        (entanglements tm-orig))
               )
          (as entanglements pt-tm-entangled
            {
              :➜ok (λ()
                       (setf (base tm-entangled) (entangle (base tm-orig)))
                       (setf (entanglements tm-entangled) entanglements)
                       (setf (address tm-entangled) (address tm-orig))
                       (setf (address-rightmost tm-entangled) (address-rightmost tm-orig))
                       [➜ok tm-entangled]
                       )
              :➜no-alloc ➜no-alloc
              })
          )))


