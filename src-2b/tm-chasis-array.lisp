#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  tm-array - tape machine based on a tape which is a tape-array
  handle-tm-array - 


  Holds the heads and the tape.  The chasis is empty when the tape is empty.  There is no 'with' scoping
  for the chasis, so we have no need for abandoned.  

  tm-array specifies a head in the tm-array-chasis.  Hence, tm-array can be parked, but tm-base itself
  can not.

  We avoid keeping entanglement lists by having multiple heads on a chasis.  However, the entanglment
  list model more resembles sharing of memory over distributed hardware.

|#


(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type
;;
  (def-type chasis-array (chasis)
    (
      (tms
        :initform ∅  ; there are no tms yet
        :accessor tms
        )
      (tape 
        :initform ∅ ; empty tape
        :accessor tape
        )
      (channel ; when we fork a tape array, we make a complete copy
        :initform 0
        :accessor channel
        )
      ))

  ;; useful conjunctions of status:

  (def-type tm-array-empty (tm-array tm-chasis-empty))
  (def-type tm-array-active (tm-array tm-chasis-active))
  
  (defun-typed to-empty     ((tm tm-array)) (change-class tm 'tm-array-empty))
  (defun-typed to-active    ((tm tm-array)) (change-class tm 'tm-array-active))

  (defun-typed init ((tm tm-array) array &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (setf (tape tm) array)
      [➜ok tm]
      ))

;;--------------------------------------------------------------------------------
;; tape operations
;;
  (defun-typed eur ((ch chasis-array) &optional ➜)
    (destructuring-bind
      (
        &key
        (address 0)
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (read<tape-array> (tape ch) {:address address})]
      ))
   
  (defun-typed euw ((ch chasis-array) instance &optional ➜)
    (destructuring-bind
      (
        &key
        (address 0)
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (write<tape-array> (tape ch) instance {:address address})
      [➜ok]
      ))


;;--------------------------------------------------------------------------------
;; absolue head control
;;
  (defun-typed p<chasis> ((ch chasis-array) head-number &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (w<tape-array> (head tm) 'parked {:address head-number})
      [➜ok]
      ))

  ;; cue the head
  (defun-typed u<chasis> ((ch chasis-array) head-number &optional ➜)
    (destructuring-bind
      (
        &key
        (address 0) ; for higher rank tapes the cell address will be a list
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (w<tape-array> (head tm) address {:address head-number})
      [➜ok]
      ))

  ;; head location
  (defun-typed @<chasis> ((ch chasis-array) head-number &optional ➜)
    (destructuring-bind
      (
        &key
        (➜parked )
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (r<tape-array> (head tm) 
        {
          :address head-number
          :➜ok (λ(instance)
                 (cond
                   ((eq instance 'parked) [➜parked])
                   (t [➜ok instance])
                   ))
          })))



;;--------------------------------------------------------------------------------
;; relative head control
;;

  (defun-typed s<chasis> ((ch chasis-array) head-number &optional ➜)
    (destructuring-bind
      (
        &key
        (Δ 1)
        (➜ok (be t))
        (➜bound #'echo)
        &allow-other-keys
        )
      ➜
      (r<tape-array> (head tm)
        {
          :address head-number
          :➜ok (λ(address)
                 (let(
                       (proposed-new-address (+ address Δ))
                       (max (max<tape-array> (tape tm)))
                       )
                   (cond
                     ((< proposed-new-address 0)
                       (w<tape-array> (head tm) 0 {:address head-number})
                       [➜bound proposed-new-address]
                       )
                     ((> proposed-new-address max)
                       (w<tape-array> (head tm) max {:address head-number})
                       [➜bound (- proposed-new-address max)]
                       )
                     (t
                       (w<tape-array> (head tm) proposed-new-address {:address head-number})
                       [➜ok]
                       )))
                 :➜empty #'cant-happen
                 })))
      
  
;;--------------------------------------------------------------------------------
;; access through head
;;
  (defun-typed r<chasis> ((ch chasis-array) head-number &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (r<tape-array> (head tm)
        {
          :address head-number
          :➜ok (λ(address)
                 (r<tape-array> (tape tm)
                   {
                     :address address
                     :➜ok ➜ok
                     :➜empty #'cant-happen
                     }))
          :➜empty #'cant-happen
          })))

  (defun-typed w ((tm tm-array-active) instance head-number &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        (➜alloc-fail #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (r<tape-array> (head tm)
        {
          :address head-number
          :➜ok (λ(address)
                 (w<tape-array> (tape tm) instance
                   {
                     :address address
                     :➜ok ➜ok
                     :➜alloc-fail ➜alloc-fail
                     }))
          :➜empty #'cant-happen
          })))
   

