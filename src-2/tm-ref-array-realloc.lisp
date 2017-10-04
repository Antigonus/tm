#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  tm-ref-array-realloc - tape machine based on a tape which is a tape-ref-array-realloc
  handle-tm-ref-array-realloc - 

  tm-ref-array-realloc specifies a head in the tm-ref-array-realloc-chasis.  Hence, tm-ref-array-realloc can be parked, but the chasis itself
  can not.

  We avoid keeping entanglement lists by having multiple 'tms' on a chasis.  Each tm has
  access to the chasis, and thus access to all tm.  So for example, if one goes empty,
  then that one can make all the others empty.  This implimentation is more compact than
  the prior entanglment list model (where each tm shared a listener list) but entanglement
  list model looks more like a simulation of sharing of memory over distributed hardware.

  Wish the head could be a pointer into the array rather than an index, as the repeated access
  calculation is a bit of a waste.  However, the ref-array-realloc base can change with expansion,
  so though indexes remain valid, pointers would not.  Perhaps we could keep a byte pointer
  and add a stride value.  A sort of base invariant pointer arithmetic ..

|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type
;;
  (def-type tm-ref-array-realloc (tm)
    (
      (chasis :accessor chasis)
      (head :accessor head)
      ))

  (def-type tm-ref-array-realloc-abandoned (tm-ref-array-realloc tm-abandoned)()) ; used by scoping operators

  ;; useful conjunctions of status:
  (def-type tm-ref-array-realloc-empty-parked-active (tm-ref-array-realloc tm-empty-parked-active)()) ; not abandoned
  (def-type tm-ref-array-realloc-empty-parked        (tm-ref-array-realloc tm-empty-parked)()) ; not active
  (def-type tm-ref-array-realloc-parked-active       (tm-ref-array-realloc tm-parked-active)()) ; not empty

  (def-type tm-ref-array-realloc-empty
    (
      tm-ref-array-realloc-empty-parked-active
      tm-ref-array-realloc-empty-parked
      tm-empty
      )
    ()
    )
  (def-type tm-ref-array-realloc-parked
    (
      tm-ref-array-realloc-empty-parked-active
      tm-ref-array-realloc-empty-parked
      tm-ref-array-realloc-parked-active
      tm-parked
      )
    ()
    )
  (def-type tm-ref-array-realloc-active
    (
      tm-ref-array-realloc-empty-parked-active
      tm-ref-array-realloc-parked-active
      tm-active
      )
    ()
    )

  (defun-typed to-abandoned ((tm tm-ref-array-realloc))
    (setf (head tm) ∅)
    (change-class tm 'tm-ref-array-realloc-abandoned)
    )
  (defun-typed to-empty     ((tm tm-ref-array-realloc))
    (setf (head tm) 'parked) ; so that there are no gc issues with head keeping data alive
    (change-class tm 'tm-ref-array-realloc-empty)
    )
  (defun-typed to-parked    ((tm tm-ref-array-realloc))
    (setf (head tm) 'parked) ; so that there are no gc issues with head keeping data alive
    (change-class tm 'tm-ref-array-realloc-parked)
    )
  (defun-typed to-active  ((tm tm-ref-array-realloc)) 
    ;; external code sets the head before calling this, the head must have an array index when active
    (change-class tm 'tm-ref-array-realloc-active)
    )

  (def-type chasis-array ()
    (
      (tms  ;; intended to be the heap that holds the tms.  Currently we emulate the heap.
        :initform ∅
        :accessor tms
        )
      (tape
        :initform ∅
        :accessor tape
        )
      ))

  (defun-typed init ((tm tm-ref-array-realloc) value &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        (➜bad-init (λ()(error 'bad-init-value)))
        &allow-other-keys
        )
      ➜
      (let(
            (chasis (make-instance 'chasis-array))
            )
        (setf (chasis tm) chasis)
        (w<tape-ref-array-realloc> (tms chasis) (tg:make-weak-pointer tm))
        (cond
          ((typep value 'null)
            (to-empty tm)
            [➜ok tm]
            )
          ((typep value 'box) ; unboxed value must be a tape-ref-array-realloc type instance!
            (setf (tape chasis) (unbox value))
            (to-parked tm)
            [➜ok tm]
            )
          (t
            [➜bad-init]
            )))))


;;--------------------------------------------------------------------------------
;; tape operations
;;
  (defun-typed eur ((tm tm-ref-array-realloc-parked-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (address 0)
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (r<tape-ref-array-realloc> (tape (chasis tm)) {:address address})]
      ))
   
  (defun-typed euw ((tm tm-ref-array-realloc-empty) instance &optional ➜)
    (destructuring-bind
      (
        &key
        (address 0)
        (➜ok (be t))
        (➜alloc-fail #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (w<tape-ref-array-realloc> (tape (chasis tm)) instance {:address address :➜alloc-fail ➜alloc-fail})
      (to-parked tm)
      [➜ok]
      ))

  (defun-typed euw ((tm tm-ref-array-realloc-parked-active) instance &optional ➜)
    (destructuring-bind
      (
        &key
        (address 0)
        (➜ok (be t))
        (➜alloc-fail #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (w<tape-ref-array-realloc> (tape (chasis tm)) instance {:address address :➜alloc-fail ➜alloc-fail})
      [➜ok]
      ))

;;--------------------------------------------------------------------------------
;; absolue head control
;;
  ;; cue the head
  (defun-typed u ((tm tm-ref-array-realloc-parked) &optional ➜)
    (destructuring-bind
      (
        &key
        (address 0) ; for higher rank tapes the cell address will be a list
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (head tm) address)
      (to-active tm)
      [➜ok]
      ))
  (defun-typed u ((tm tm-ref-array-realloc-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (address 0) ; for higher rank tapes the cell address will be a list
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (head tm) address)
      [➜ok]
      ))

  ;; head address
  ;; for a multidimensional base array the address will be a list
  (defun-typed @ ((tm tm-ref-array-realloc-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (head tm)] ; tape array head is the location
      ))


;;--------------------------------------------------------------------------------
;; relative head control
;;
  ;; usually Δ is a constant, so I expect that the optimizing compiler reduces this code
  ;; after I get the funciton inlining in place. .. can we inline a dispatched function .. hmmm
  (defun-typed s ((tm tm-ref-array-realloc-parked) &optional ➜)
    (destructuring-bind
      (
        &key
        (Δ 1)
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (cond
        ((> Δ 0)
          (setf (head tm) 0)
          (to-active tm)
          (if (> Δ 1)
            (s tm {:Δ (1- Δ) (o ➜)})
            [➜ok]
            ))
        ((< Δ 0)
          (setf (head tm) (max<tape-ref-array-realloc> (tape (chasis tm))))
          (to-active tm)
          (if (< Δ -1)
            (s tm {:Δ (1+ Δ) (o ➜)})
            [➜ok]
            ))
        (t
          [➜ok]
          ))))

  (defun-typed s ((tm tm-ref-array-realloc-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (Δ 1)
        (➜ok (be t))
        (➜bound (be ∅))
        &allow-other-keys
        )
      ➜
      (let(
            (proposed-new-address (+ (head tm) Δ))
            (max (max<tape-ref-array-realloc> (tape (chasis tm))))
            )
        (cond
          ((∨ (< proposed-new-address 0)(> proposed-new-address max))
            [➜bound]
            )
          (t
            (setf (head tm) proposed-new-address)
            [➜ok]
            )))))
  
;;--------------------------------------------------------------------------------
;; access through head
;;
  (defun-typed r ((tm tm-ref-array-realloc-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (r<tape-ref-array-realloc> (tape (chasis tm))
        {
          :address (head tm)
          :➜ok ➜ok
          :➜empty #'cant-happen
          })))

  (defun-typed w ((tm tm-ref-array-realloc-active) instance &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        (➜alloc-fail #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (w<tape-ref-array-realloc> (tape (chasis tm)) instance
        {
          :address (head tm)
          :➜ok ➜ok
          :➜alloc-fail ➜alloc-fail
          :➜empty #'cant-happen
          })))
   


;;--------------------------------------------------------------------------------
;; copy
;;

#|

  (defun-typed entangle ((tm tm-ref-array-realloc) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let(
            (chasis (chasis tm))
            (new-tm (make-instance 'tm-ref-array-realloc))
            )
        (setf (chasis new-tm) chasis)
        (setf (head new-tm) (head tm)) ; starts in the same place

        ;; ok now we need to add the new-tm to the chasis tm list
        ;; we will try to use a stale weak-pointer, but failing that will append to the list
        (let(
              (i 0)
              (max (max<tape-ref-array-realloc> (tms chasis)
              )
          (proposed-tm (r<tape-ref-array-realloc> (tms chasis)))
          (⟳(λ(➜again)
              (cond
                ((tg:weak-pointer-value proposed-tm)
                  (incf i)
                  [➜again]
                  )
                (t
                  
                

              ))
        (a◨<tape-ref-array-realloc> (tms chasis) (tg:make-weak-pointer new-tm))
        [➜ok new-tm]
        )))

  (defun-typed fork ((tm tm-ref-array-realloc) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let(
            (tape (tape (chasis tm)))
            (new-tape ∅)
            )
        (let(
              (i (max<tape-ref-array-realloc> tape))
              )
          (⟳(λ(➜again) ; important to write max address first, so that the new-tape doesn't repeatedly expand
              (w<tape-ref-array-realloc> new-tape (r<tape-ref-array-realloc> tape {:address i}))
              (when (> i 0)
                (decf i)
                [➜again]
                ))))
        (let(
              (new-chasis (make-instance 'chasis))
              (new-tm (make-instance 'tm))
              )
          (setf (tape new-chasis) new-tape)
          (setf (chasis new-tm) new-chasis)
          (setf (head new-tm) (head tm))
          (a◨<tape-ref-array-realloc> (tms new-chasis) (tg:make-weak-pointer new-tm))
          [➜ok new-tm]
          ))
      ))
|#

