#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; initialize a tape machine of the specified type to hold the specified objects
;;
;;  init-list is a keyword list.  
;;
  (defgeneric init (instance init-list &optional cont-ok cont-fail &rest ⋯))

  (defun mk (tm-type &rest init-list)
    (let(
          (instance (make-instance tm-type))
          )
      (init instance init-list)
      instance
      ))

;;--------------------------------------------------------------------------------
;;  given a sequence, returns a tape machine over that sequence
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

