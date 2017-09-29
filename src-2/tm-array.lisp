#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  tm-array - tape machine based on a tape which is a tape-array
  handle-tm-array - 

  tm-array specifies a head in the tm-array-chasis.  Hence, tm-array can be parked, but the chasis itself
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
  (def-type tm-array (tm)
    (
      (chasis :accessor chasis)
      (head :accessor head)
      ))

  (def-type tm-array-abandoned (tm-array tm-abandoned)()) ; used by scoping operators

  ;; useful conjunctions of status:
  (def-type tm-array-empty-parked-active (tm-array tm-empty-parked-active)()) ; not abandoned
  (def-type tm-array-empty-parked        (tm-array tm-empty-parked)()) ; not active
  (def-type tm-array-parked-active       (tm-array tm-parked-active)()) ; not empty

  (def-type tm-array-empty
    (
      tm-array-empty-parked-active
      tm-array-empty-parked
      )
    (tm-empty)
    )
  (def-type tm-array-parked
    (
      tm-array-empty-parked-active
      tm-array-empty-parked
      tm-array-parked-active
      )
    (tm-parked)
    )
  (def-type tm-array-active
    (
      tm-array-empty-parked-active
      tm-array-parked-active
      )
    (tm-active)
    )

  (defun-typed to-abandoned ((tm tm-array))
    (setf (head tm) ∅)
    (change-class tm 'tm-array-abandoned)
    )
  (defun-typed to-empty     ((tm tm-array))
    (setf (head tm) 'parked) ; so that there are no gc issues with head keeping data alive
    (change-class tm 'tm-array-empty)
    )
  (defun-typed to-parked    ((tm tm-array))
    (setf (head tm) 'parked) ; so that there are no gc issues with head keeping data alive
    (change-class tm 'tm-array-parked)
    )
  (defun-typed to-active  ((tm tm-array)) 
    ;; external code sets the head before calling this, the head must have an array index when active
    (change-class tm 'tm-array-active)
    )

  (def-type chasis-array ()
    (
      (tms
        :initform ∅  ; there are no tms yet
        :accessor tms
        )
      (tape 
        :initform ∅ ; empty tape
        :accessor tape
        )
      ))

  (defun-typed init ((tm tm-array) (value null) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (to-empty tm)
      (let(
            (chasis (make-instance 'chasis-array))
            )
        (setf (chasis tm) chasis)
        (w<tape-array> (tms chasis) tm)
        [➜ok tm]
        )))

;;--------------------------------------------------------------------------------
;; tape operations
;;
  (defun-typed eur ((tm tm-array-parked-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (address 0)
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (r<tape-array> (tape (chasis tm)) {:address address})]
      ))
   
  (defun-typed euw ((tm tm-array-parked-active) instance &optional ➜)
    (destructuring-bind
      (
        &key
        (address 0)
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (w<tape-array> (tape (chasis tm)) instance {:address address})
      [➜ok]
      ))

;;--------------------------------------------------------------------------------
;; absolue head control
;;
  ;; cue the head
  (defun-typed u ((tm tm-array-parked) &optional ➜)
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
  (defun-typed u ((tm tm-array-active) &optional ➜)
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

  (defun-typed abandon ((tm tm-array))
    ;; add clean to the gc hook
    (to-abandoned tm)
    )

  ;; head address
  ;; for a multidimensional base array the address will be a list
  (defun-typed @ ((tm tm-array-active) &optional ➜)
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
  (defun-typed s ((tm tm-array-active) &optional ➜)
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
            (max (max<tape-array> (tape (chasis tm))))
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
  (defun-typed r ((tm tm-array-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (r<tape-array> (tape (chasis tm))
        {
          :address (head tm)
          :➜ok ➜ok
          :➜empty #'cant-happen
          })))

  (defun-typed w ((tm tm-array-active) instance &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        (➜alloc-fail #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (w<tape-array> (tape (chasis tm)) instance
        {
          :address (head tm)
          :➜ok ➜ok
          :➜alloc-fail ➜alloc-fail
          :➜empty #'cant-happen
          })))
   

;;--------------------------------------------------------------------------------
;; copy
;;
  (def-function-class entangle (tm &optional ➜))
  (def-function-class fork (tm &optional ➜))

  (defun-typed entangle ((tm tm-array) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let(
            (chasis (chasis tm))
            (new-tm (make-instance 'tm-array))
            )
        (setf (head new-tm) (head tm))
        (a◨<tape-array> (tms chasis) new-tm)
        [➜ok tm]
        )))

  (defun-typed fork ((tm tm-array) &optional ➜)
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
              (i (max<tape-array> tape))
              )
          (⟳(λ(➜again) ; important to write max address first, so that the new-tape doesn't repeatedly expand
              (w<tape-array> new-tape (r<tape-array> tape {:address i}))
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
          (a◨<tape-array> (tms new-chasis) new-tm)
          ))
      [➜ok tm]
      ))



