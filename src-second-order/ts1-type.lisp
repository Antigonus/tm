#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  A thread safe version of the ea machine.

  We keep the entangelements machine so as to know:
   1. which machines to make empty, or not empty
   2. which machines to update their leftmost cell pointer for a◧  or d◧
   3. which machines to check for a collision before deleteing a cell

  Ea already synchronizes with the garbage collector calling the finalizers. Finalizers
  delete weak pointers out of the entanglements list, and can be called at any time.
  In the ea machine, users must own the entanglements machine before using it.  Ownership
  is demonstrated by 'acquiring the entanglements lock'. 

  With multiple threads we must also synchronize between certain interface
  functions, so as to avoid these hazards:
   1. the machine is adding or deleting a cell, and another machine
      gets messed up in the construction work.  This could happen with head
      motion or competing structural operations.
   2. address incf/decf gets the wrong answer, because between the read and write of
      the incremented address, the address value was written.

  We discuss various approaches to avoiding these hazards in
  doc/implementation/multiple-threads.txt This is an implementation of the first algorithm
  discussed there.  Accordingly, we use the entanglement's lock as a general tape
  ownership lock, and require any operation that performs head motion or structural
  operations to own this lock.



|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-type ts1-tm (status-tm)
    (
      (ownership
        :initarg :ownership
        :accessor base
        )
      ))

  (def-type ts1-abandoned (ts1-tm status-abandoned)())
  (def-type ts1-active    (ts1-tm status-active)())
  (def-type ts1-empty     (ts1-tm status-empty)())
  (def-type ts1-parked    (ts1-tm status-parked)())

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
      (tm status-tm)
      (keyed-parms cons)
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜fail (λ()(error 'bad-init-value)))
        &allow-other-keys
        )
      ➜
      (call-next-method tm ➜
        {
          :➜ok (λ()
                 (setf (ownership tm) (bt:make-lock))
                 [➜ok]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })))

  (defun-typed entangle ((tm-orig status-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        ;; (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜  
      (let(
            have-lock
            )
        (bt:acquire-lock lock)
        (setf have-lock t)
        (call-next-method tm-orig
          {
            :➜ok (λ(tm-entangled)
                   (setf (entanglements 
                   (setf (address tm-entangled) (address tm-orig))
                   (setf (address-rightmost tm-entangled) (address-rightmost tm-orig))
                   [➜ok tm-entangled]
                   )
            (o (remove-key-pair ➜ :➜ok))
            })))
