#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  A new non-destructive tape machine may be made by calling nd-mk.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; tape machine states
;;

  ;; a void machine has no tape and no head
  (defparam void (make-instance 'void))

  ;; a parked machine has a tape, but no head
  (defparam parked (make-instance 'parked))

  ;; a parked machine has a tape, but no head
  (defparam abandoned (make-instance 'abandoned))

  ;; an active machine has both a tape and a head
  (defparam active (make-instance 'active))

;;--------------------------------------------------------------------------------
;; initialize a tape machine of the specified type to hold the specified objects
;;
;;  init-list is a keyword list.  
;;
;;  :tm-type is used by empty to know the tape of tape to create should cells be
;;    added to the empty.
;;
;;  :mount is used to specify initial objects for a tape machine.  
;;
;;  :seed is used to give seed parameters to generators.
;;
;;  :base provides the base machine for a transforms.
;;
;;  :rightmost marks the rightmost cell for regions
;;
;;  Other keywords should not override these, as they are used in the init
;;  routines to detect valid initialization expressions.
;;
  (defgeneric init (instance init-list &optional cont-ok cont-fail))

  (defmethod init 
    (
      (instance nd-tm-machine)
      init-list
      &optional 
      (cont-ok (be t))
      (cont-fail (λ()(error 'unrecognized-instance-type)))
      )
    (declare (ignore instance init-list cont-ok))
    (funcall cont-fail)
    )

  ;; tm-type is nd-tape-machine or derived from nd-tape-machine
  (defun nd-mk (tm-type &rest init-list)
    (let(
          (instance (make-instance tm-type))
          )
      (init instance init-list)
      instance
      ))

;;--------------------------------------------------------------------------------
;;  given a sequence return a tape machine over that sequence
;;    sequences are things we can step into, and that tree traversal will
;;    consider to be something to traverse.
;;
;;  tm-derived provides a mount for tape machines (similar to fork but cues to leftmost)
;;
  (defgeneric mount (sequence &optional cont-ok cont-fail))

  (defmethod mount
    (
      sequence 
      &optional 
      (cont-ok #'echo)
      (cont-fail (λ()(error 'mount-unrecognized-sequence-type)))
      )
    (declare (ignore sequence cont-ok))
    (funcall cont-fail)
    )

