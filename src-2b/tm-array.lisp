#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Implementation of a tape machine over a tape-array

  So as to support entanglement, we make head into a tape-array of heads.
  Consequently we must provide a head number for functions that use the head.
  A parked head is given the value 'parked, so that the head is not locking a
  cell in memory.  

|#


(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type
;;
  (def-type tm-array (tm)
    (
      (head
        :initform ∅  ; there are no heads yet
        :accessor head
        )
      (tape 
        :initform ∅
        :accessor tape
        )
      (channel ; when we fork a tape array, we make a complete copy
        :initform 0
        :accessor channel
        )
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
  
  (defun-typed to-abandoned ((tm tm-array)) (change-class tm 'tm-array-abandoned))
  (defun-typed to-empty     ((tm tm-array)) (change-class tm 'tm-array-empty))
  (defun-typed to-parked    ((tm tm-array)) (change-class tm 'tm-array-parked))
  (defun-typed to-active    ((tm tm-array)) (change-class tm 'tm-array-active))

  (defun-typed init ((tm tm-array) array &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (w<tape-array> (head tm) 'parked)
      (setf (tape tm) array)
      [➜ok tm]
      ))

;;--------------------------------------------------------------------------------
;; tape operations
;;
  (defun-typed eur ((tm tm-array-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (address 0)
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (read<tape-array> (tape tm) {:address address})]
      ))
   
  (defun-typed euw ((tm tm-array-active) instance &optional ➜)
    (destructuring-bind
      (
        &key
        (address 0)
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (write<tape-array> (tape tm) instance {:address address})
      [➜ok]
      ))


;;--------------------------------------------------------------------------------
;; absolue head control
;;
  (defun-typed p ((tm tm-array-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (head 0)
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (w<tape-array> (head tm) 'parked {:address head})
      (to-parked tm)
      [➜ok]
      ))

  ;; cue the head
  (defun-typed u ((tm tm-array-parked-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (head 0)
        (address 0) ; for higher rank tapes the cell address will be a list
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (w<tape-array> (head tm) address {:address head})
      [➜ok]
      ))

  ;; head location
  (defun-typed @ ((tm tm-array-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (head 0)
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (r<tape-array> (head tm) {:address head})]
      ))

;;--------------------------------------------------------------------------------
;; relative head control
;;

  ;; stepping from parked cues the machine
  (defun-typed s ((tm tm-array-parked) &optional ➜)
    (u tm ➜)
    )
  (defun-typed s ((tm tm-array-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (Δ 1)
        (head 0)
        (➜ok (be t))
        (➜bound #'echo)
        &allow-other-keys
        )
      ➜
      (r<tape-array> (head tm)
        {
          :address head
          :➜ok (λ(address)
                 (let(
                       (proposed-new-address (+ address Δ))
                       (max (max<tape-array> (tape tm)))
                       )
                   (cond
                     ((< proposed-new-address 0)
                       (w<tape-array> (head tm) 0 {:address head})
                       [➜bound proposed-new-address]
                       )
                     ((> proposed-new-address max)
                       (w<tape-array> (head tm) max {:address head})
                       [➜bound (- proposed-new-address max)]
                       )
                     (t
                       (w<tape-array> (head tm) proposed-new-address {:address head})
                       [➜ok]
                       )))
                 :➜empty #'cant-happen
                 })))
      
  
;;--------------------------------------------------------------------------------
;; access through head
;;
  (defun-typed r ((tm tm-array-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (head 0)
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (r<tape-array> (head tm)
        {
          :address head
          :➜ok (λ(address)
                 (r<tape-array> (tape tm)
                   {
                     :address address
                     :➜ok ➜ok
                     :➜empty #'cant-happen
                     }))
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
      (r<tape-array> (head tm)
        {
          :address head
          :➜ok (λ(address)
                 (w<tape-array> (tape tm) instance
                   {
                     :address address
                     :➜ok ➜ok
                     :➜alloc-fail ➜alloc-fail
                     }))
          :➜empty #'cant-happen
          })))
   

