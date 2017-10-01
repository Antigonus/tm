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
  calculation is a bit of a waste.  However, there is no pointer arithmetic in Lisp.

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
      (tms 
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
   




