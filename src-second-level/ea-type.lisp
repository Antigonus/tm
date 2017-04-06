#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  There is a synchronizaiton problem when machines are entangled.  When we have
  a set of entangled machines, when any member of that set becomes empty, all members
  of the set are empty.  Before deleting a cell from the tape while using one machine,
  we must check that no other machine has a head on that cell.  etc.

  Each ea-tm keeps a reference to a shared tape listener list called, 'entanglements', and
  a copy of a pointer to its entry in that list.

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
      (entanglements-pt ; our location in the entangelments list
        :initarg :entanglements-pt
        :accessor entanglements-pt
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
      &optional
      init-parms
      ➜
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
                 (mk 'bilist-haz-tm {:tape {tm}}
                   {
                     :➜ok (λ(entanglements)
                            (setf (entanglements-pt tm) (entangle entanglements))
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
  ;;
    (defun-typed entangle ((tm-orig ea-tm) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          (➜no-alloc #'alloc-fail)
          &allow-other-keys
          )
        ➜  
        ;; (prins (print "entangle ea-tm"))
        (call-next-method tm-orig
          {:➜ok
            (λ(tm-entangled)
              (let(
                    (entanglements (entanglements tm-orig))
                    )
                (as entanglements tm-entangled
                  {
                    :➜ok (λ()
                           (setf (entanglements tm-entangled) entanglements)
                           (setf (entanglements-pt tm-entangled) (entangle entanglements))
                           [➜ok tm-entangled]
                           )
                    :➜no-alloc ➜no-alloc
                    })))
            (o (remove-key-pair ➜ :➜ok))
            })))


;;--------------------------------------------------------------------------------
;;
  (defun-typed abandon ((tm ea-tm))
    (let(
          (e-pt (entanglements-pt tm))
          )
      (d. e-pt)
      (setf (entanglements  tm) ∅)
      (setf (entanglements-pt tm) ∅)
      (call-next-method tm)
      ))
            

