#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Manager synchronizes operation on tape machines.

  Garbage collection finalization, I might assume occurs on a separate thread, and thus
  use of the entanglements list is made thread safe. Though use of the entanglements list
  is thread safe, the use of base machine is not.  We can not, for exampe, delete a 
  cell on one thread while moving the head on another thread.  For a thread safe
  entanglements account machine use a 'ts' (thread safe) machine.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-type entangelements-type ()
    (
      (lock :initarg lock :accessor lock)
      (instances :initarg instances :accessor instances) ; instances are machines that share a tape
      ))

  (def-type ea-tm (status-tm)
    (
      (entanglements
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
                 (mk 'list-haz-tm {:tape {(tg:make-weak-pointer tm)}}
                   {
                     :➜ok (λ(a-haz-tm)
                            (let(
                                  (entanglements (make-instance 'entangelements-type))
                                  )
                              (setf (lock entanglements) (bt:make-lock))
                              (setf (instances entanglements) a-haz-tm)
                              (setf (entanglements tm) entanglements)
                              )
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

  ;; accepts an instance (typep 'ea-tm), returns an entangled instance of the same type
  ;;
  (defun-typed entangle ((tm-orig ea-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        ;; (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜  
      (let*(
             (i (make-instance (type-of tm-orig)))
             (pt-i (tg:make-weak-pointer i))
             (entanglements (entanglements tm-orig))
             (instances (instances entanglements))
             (lock (lock entanglements))
             )
        (bt:with-lock-held (lock)
          (a instances pt-i
            {
              :➜ok (λ()
                     (let*(
                            (d-pt (entangle instances)); used to deleted the cell holding i
                            (finalize-i (λ()
                                          (bt:with-lock-held ((lock entanglements))
                                            (c◧ instances); d can never collide with leftmost
                                            (d d-pt)
                                            )))
                            )
                       (tg:finalize i finalize-i)
                       (setf (base i) (entangle (base tm-orig)))
                       (setf (entanglements i) entanglements)
                       (setf (address i) (address tm-orig))
                       (setf (address-rightmost i) (address-rightmost tm-orig))
                       [➜ok i]
                       ))
              (o (remove-key-pair ➜ :➜ok))
              })))))


