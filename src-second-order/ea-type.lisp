#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Manager synchronizes operation on tape machines.

  Garbage collection finalization, I might assume occurs on a separate thread, and thus
  use of the entanglements list must be thread safe.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-type ent-obj-type ()
    (
      (lock :initarg lock :accessor lock)
      (tm-of-entangled-tms :initarg tm-of-entangled-tms :accessor tm-of-entangled-tms) ; instances are machines that share a tape
      ))

  (def-type ea-tm (status-tm)
    (
      (entanglements ; an instance of an ent-obj-type
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
                 (mk 'list-haz-tm {:tape {tm}}
                   {
                     :➜ok (λ(a-haz-tm)
                            (let(
                                  (ents-obj (make-instance 'ent-obj-type))
                                  )
                              (setf (lock ents-obj) (bt:make-lock))
                              (setf (tm-of-entangled-tms ents-obj) a-haz-tm)
                              (setf (entanglements tm) ents-obj)
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
             (ents-obj (entanglements tm-orig))
             (tm-of-entangled-tms (tm-of-entangled-tms ents-obj))
             (lock (lock ents-obj))
             )
        (bt:with-lock-held (lock)
          (a tm-of-entangled-tms pt-i
            {
              :➜ok (λ()
                     (let*(
                            (d-pt (entangle tm-of-entangled-tms))
                            (finalize-i (λ()
                                          (cue-leftmost tm-of-entangled-tms)
                                          (d d-pt) ; d can never collide with leftmost
                                          ))
                            )
                       (tg:finalize i finalize-i)
                       (setf (base i) (entangle (base tm-orig)))
                       (setf (entanglements i) ents-obj)
                       (setf (address i) (address tm-orig))
                       (setf (address-rightmost i) (address-rightmost tm-orig))
                       [➜ok i]
                       ))
              (o (remove-key-pair ➜ :➜ok))
              })))))


