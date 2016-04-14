#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  A new tape machine may be made by calling tm-mk.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; the base type
;;
  (defclass tape-machine ()
    (
      (HA 
        :initarg :HA 
        :accessor HA
        )
      (tape
        :initarg :tape
        :accessor tape
        )
      ))

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
;;  Other keywords should not override these, as they are used in the init
;;  routines to detect valid initialization expressions.
;;
  (defgeneric init (instance init-list &optional cont-ok cont-fail))

  (defmethod init 
    (
      instance 
      init-list
      &optional 
      (cont-ok (be t))
      (cont-fail (λ() (error 'unrecognized-instance-type)))
      )
    (declare (ignore instance init-list cont-ok))
    (funcall cont-fail)
    )

  (defun mk (tm-type &rest init-list)
    (let(
          (instance (make-instance tm-type))
          )
      (init instance init-list)
      instance
      ))

;;-----------------------p---------------------------------------------------------
;;  given a sequence return a tape machine over that sequence
;;    sequences are things we can step into, and that tree traversal will
;;    consider to be something to traverse.
;;
;;  note that tm-derived provides a generic mount for mounting a 
;;  tape machine to a tape machine, which is similar to dup, except the
;;  head goes to leftmost
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

