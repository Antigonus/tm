#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make array machines.



|#

(in-package #:tm)


;; walks the tiles in an address
(def-type tiled-number-tm ()
  (
    (head ; current bit location
      :initarg :head 
      :accessor head
      )
    (tape ; numeric-address
      :initarg numeric-address
      :initarg numeric-address
      )))





;;--------------------------------------------------------------------------------
;; a specialization
;;
  (def-type array ()
    (
      (directory ; an array type
        :initarg directory
        :initarg directory
        )))


;;--------------------------------------------------------------------------------
;; a specialization
;;
  (def-type array-tm (tape-machine)
    (
      (head ; locates a cell on the tape
        :initarg :head 
        :accessor head
        )
      (tape ; points to the top level directory
        :initarg :tape
        :accessor tape
        )
      ))


  (defun-typed r ((tm array-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      ()

      ))



#|

;;--------------------------------------------------------------------------------
;;  Make array-tm machines. Typically the tm will be a array-nd-tm or a array-solo-tm
;;  instance, and then this gets called due to the inheritance structure.
;;
  (defun-typed init 
    (
      (tm array-tm)
      &optional 
      keyed-parms
      ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys          
        )
      ➜
      (destructuring-bind
        (&key tape) keyed-parms
        (cond
          ((∧ tape (consp tape))
            (setf (head tm) tape)
            (setf (tape tm) tape)
            [➜ok tm]
            )
          (t
            (setf (tape tm) (cons ∅ ∅))
            (setf (head tm) (tape tm))
            (call-next-method keyed-parms ➜) ; pick up tape-machine's init for non consp tapes
            ))
        )))
|#
