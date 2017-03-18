#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  ea2 supports asynchronous 'finalization' for garbage collecting weak pointers
  out of entanglements machines.

  ea2 and the finalizers co-ordinate the use the entanglements machine by using a mutex.
  Before any head motion or structural operation is performed on entanglements, the thread
  must own the entanglements machine, and ownership is demonstrated by holding the
  'lock'.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-type locked-entanglements-type ()
    (
      (lock :initarg lock :accessor lock)

      ;; entanglements is a tm-haz machine. We use a mutex to avoid hazards.
      ;; Instances on the entanglements machine are weak pointers to the entangled machines.
      ;; We use finalizers to remove disengaged weak pointers
      (entanglements :initarg entanglements :accessor entanglements) 
      ))

  (def-type ea2-tm (status-tm)
    (
      (locked-entanglements
        :initarg :locked-entanglements
        :accessor locked-entanglements
        )
      ))

  (def-type ea2-abandoned (ea2-tm status-abandoned)())
  (def-type ea2-active    (ea2-tm status-active)())
  (def-type ea2-empty     (ea2-tm status-empty)())
  (def-type ea2-parked    (ea2-tm status-parked)())

;;--------------------------------------------------------------------------------
;; state transition functions
;;
  (defun-typed to-abandoned ((tm ea2-tm)) (change-class tm 'ea2-abandoned))
  (defun-typed to-active    ((tm ea2-tm)) (change-class tm 'ea2-active))
  (defun-typed to-empty     ((tm ea2-tm)) (change-class tm 'ea2-empty))
  (defun-typed to-parked    ((tm ea2-tm)) (change-class tm 'ea2-parked))

;;--------------------------------------------------------------------------------
;;
  (defun-typed init
    (
      (tm ea2-tm)
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
                 (mk 'bilist-haz-tm {:tape {(tg:make-weak-pointer tm)}}
                   {
                     :➜ok (λ(a-haz-tm)
                            (let(
                                  (locked-entanglements (make-instance 'locked-entanglements-type))
                                  )
                              (setf (lock locked-entanglements) (bt:make-lock))
                              (setf (entanglements locked-entanglements) a-haz-tm)
                              (setf (locked-entanglements tm) locked-entanglements)
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

  ;; accepts an instance (typep 'ea2-tm), returns an entangled instance of the same type
  ;;
  ;; can the finalizer be called to delete the last cell of entanglements?  If so this
  ;; would mean we have garbage collected the last machine in the entanglement group.
  ;; But that would mean the entanglements object itself would be due to for garbage
  ;; collection, or rather it would be, but the finalizer is pointing at it.
  ;;
  (defun-typed entangle ((tm-orig ea2-tm) &optional ➜)
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
             (locked-entanglements (locked-entanglements tm-orig))
             (lock                 (lock locked-entanglements))
             (entanglements        (entanglements locked-entanglements))
             have-lock ; after we release, others might set the lock, so we need our own flag
             )
        (bt:acquire-lock lock)
        (setf have-lock t)
        (unwind-protect ; prevents orphaning the lock due to unexpected excdeptions
          (as entanglements pt-tm-entangled
            {
              :➜ok (λ()
                     (let*(
                            (d-pt (entangle entanglements)); points to cell to be deleted
                            (finalizer (λ()
                                         (bt:with-lock-held (lock)
                                           (c◧ entanglements)
                                           (on-leftmost d-pt
                                             {
                                               :➜t (λ()
                                                     (s d-pt
                                                       {
                                                         :➜ok (λ()(d◧ entanglements))
                                                         :➜rightmost #'do-nothing
                                                         }))
                                               :➜f (λ()(d d-pt))
                                               }))
                                         ))
                            )
                       (bt:release-lock lock)
                       (setf have-lock ∅)

                       ;; guesss we better initialize the entangled copy of tm-orig
                       (tg:finalize tm-entangled finalizer)
                       (setf (base                 tm-entangled) (entangle (base tm-orig)))
                       (setf (locked-entanglements tm-entangled) locked-entanglements)
                       (setf (address              tm-entangled) (address tm-orig))
                       (setf (address-rightmost    tm-entangled) (address-rightmost tm-orig))
                       [➜ok tm-entangled]
                       ))
              :➜no-alloc (λ()
                           (bt:release-lock lock) 
                           (setf have-lock ∅)
                           [➜no-alloc]
                           )
              })
          (when have-lock (bt:release-lock lock))
          ))))


