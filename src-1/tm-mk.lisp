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
        :initform 'parked
        :initarg :HA 
        :accessor HA
        )
      (tape
        :initform ∅
        :initarg :tape
        :accessor tape
        )
      ))

;;--------------------------------------------------------------------------------
;; initialize a tape machine of the specified type to hold the specified objects
;;
;;
  (defgeneric tm-init (instance init-list))

  (defmethod tm-init (instance init-list)
    (declare (ignore instance init-list))
    (error 'tm-init-unrecognized-instance-type)
    )

  (defun tm-mk (tm-type &rest init-list)
    (let(
          (instance (make-instance tm-type))
          )
      (tm-init instance init-list)
      instance
      ))

;;--------------------------------------------------------------------------------
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
       cont-ok 
       (cont-fail (error 'mount-unrecognized-sequence-type))
       )
     (declare (ignore sequence cont-ok))
     (funcall cont-fail)
     )

