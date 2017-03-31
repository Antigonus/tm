#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list machines.

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; a specialization
;;
  (def-type list-tm (tape-machine)
    (
      (head ; locates a cell on the tape
        :initarg :head 
        :accessor head
        )
      (tape ; a sequence of cells, each that may hold an instance
        :initarg :tape
        :accessor tape
        )
      ))

;;--------------------------------------------------------------------------------
;;  Make list-tm machines. Typically the tm will be a list-nd-tm or a list-solo-tm
;;  instance, and then this gets called due to the inheritance structure.
;;
  (defun-typed init 
    (
      (tm list-tm)
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
            (call-next-method keyed-parms ➜) ; pick up tape-machine's init for non consp tapes
            ))
        )))
