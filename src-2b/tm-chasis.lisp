#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  The chasis for our tape machine.  See tm.lisp for the mechanism that moves the head.

  Status:
     empty - the tape is empty
     active - tape is not empty

|#


(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type
;;
  (def-type tm-chasis ()())

  (def-type tm-chasis-empty ()())
  (def-type tm-chasis-active ()())
  
  ;; changing the machine status, these are private
  (def-function-class to-active (tm-chasis))
  (def-function-class to-empty (tm-chasis))

  (defun-typed to-empty     ((tm tm)) (change-class tm 'tm-chasis-empty))
  (defun-typed to-active    ((tm tm)) (change-class tm 'tm-chasis-active))


;;--------------------------------------------------------------------------------
;; tape operations
;;
  (def-function-class eur (tm &optional ➜)
    (:documentation
      "Reads the cell at :address.  :address defaults to 0.
       "))
  (defun-typed eur ((tm tm-chasis-empty) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))

  (def-function-class euw (tm instance &optional ➜)
    (:documentation
      "Writes the cell at :address. :address defaults to 0.
       "))
  (defun-typed euw ((tm tm-chasis-empty) instance &optional ➜)
    (declare (ignore tm instance ➜))
    (destructuring-bind
      (
        &key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))

  (def-function-class epa (tm &optional ➜)
    (:documentation
      "Prepends a new leftmost cell or cells. 
      :instance is a new instance.
      :fill  
       "))

  (def-function-class ep-a (tm instance &optional ➜)
    (:documentation
      "Appends a new rightmost cell or cells.
       "))

  (def-function-class epd (tm &optional ➜)
    (:documentation
      "Deletes the leftmost cell. Returns the instance.
       "))

  (def-function-class ep-d (tm &optional ➜)
    (:documentation
      "Deletes the rightmost cell.
       "))


;;--------------------------------------------------------------------------------
;; absolute head control
;;
  (def-function-class p<chasis> (tm head-number &optional ➜)
    (:documentation
      "Parks the specified head.
       "))
  ;; an empty machine is already parked
  (defun-typed p<chasis> ((tm tm-chasis-empty) head-number &optional ➜)
    (declare (ignore head-number))
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      [➜ok]
      ))
  (defun-typed p<chasis> ((tm tm-chasis-active) head-number &optional ➜)
    (declare (ignore head-number))
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (to-parked tm)
      [➜ok]
      ))

  ;; cue - functions that use cue will accept a cue function parameter so the user may use their own
  ;; defaults to the leftmost of the tape
  (def-function-class u<chasis> (tm head-number &optional ➜)
    (:documentation
      "Cue the specified head to specific cell.
       "))
  ;; there is nowhere possible to cue the head when the tape is empty
  (defun-typed u<chasis> ((tm tm-chasis-empty) head-number &optional ➜)
    (declare (ignore head-number))
    (destructuring-bind
      (
        &key
        (➜bound (be ∅))
        &allow-other-keys
        )
      ➜
      [➜bound]
      ))

  (def-function-class @<chasis> (tm head-number &optional ➜)
    (:documentation
      "Location of the head as a natural number, or list of natural numbers.
       "))
  (defun-typed @<chasis> ((tm tm-chasis-empty) head-number &optional ➜)
    (declare (ignore head-number))
    (destructuring-bind
      (
        &key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed @<chasis> ((tm tm-chasis-parked) head-number &optional ➜)
    (declare (ignore head-number))
    (destructuring-bind
      (
        &key
        (➜parked (λ()(error 'accessed-parked)))
        &allow-other-keys
        )
      ➜
      [➜parked]
      ))

;;--------------------------------------------------------------------------------
;; relative head control
;;
  ;; step, user will sometimes provide their own step function
  (def-function-class s<chassis> (tm head-number &optional ➜)
    (:documentation
      "Steps the specified head to a neighboring cell.
       "))
  (defun-typed s<chasis> ((tm tm-chasis-empty) head-number &optional ➜)
    (declare (ignore head-number))
    (destructuring-bind
      (
        &key
        (➜bound (be ∅))
        &allow-other-keys
        )
      ➜
      [➜bound]
      ))
  
;;--------------------------------------------------------------------------------
;; access through head
;;
  (def-function-class r<chasis> (tm head-number &optional ➜)
    (:documentation
      "Reads the cell under the specified head.
       "))
  (defun-typed r ((tm tm-chasis-empty) head-number &optional ➜)
    (declare (ignore head-number))
    (destructuring-bind
      (
        &key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))

  (def-function-class w (tm instance &optional ➜)
    (:documentation
      "Reads the cell under the head.
       "))
  (defun-typed w ((tm tm-chasis-empty) instance head-number &optional ➜)
    (declare (ignore head-number))
    (destructuring-bind
      (
        &key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))

