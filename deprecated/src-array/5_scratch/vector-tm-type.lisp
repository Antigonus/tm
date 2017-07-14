#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make vector machines.

  A vector has fixed length, and no cell can be empty.  The initial vector contents is
  given to init.  Lisp vectors already know their length, so we do not need a slot for
  that.  If we used C arrays we would have to add a length slot.  Even if we implemented
  scoping of some sort, such that all vectors in, say, a 'vector-4' environment, had
  length 4, we would still need a reference to the environment.

  


|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; a specialization
;;
  (def-type vector-tm (tape-machine)
    (
      (head ; locates a cell on the tape
        :initarg :head 
        :accessor head
        )
      (tape ; a sequence of cells, each that may hold an instance
        :initarg :tape
        :accessor tape
        )
      (size
        :initarg :size
        :accessor size
        )
      ))

;;--------------------------------------------------------------------------------
;;  Make vector-tm machines. Typically the tm will be a vector-nd-tm or a vector-solo-tm
;;  instance, and then this gets called due to the inheritance structure.
;;
  (defun-typed init 
    (
      (tm vector-tm)
      &optional 
      keyed-parms
      ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜fail (error 'bad-init-value))
        &allow-other-keys          
        )
      ➜
      (destructuring-bind
        (&key tape length) keyed-parms
        (if (¬ length)
          [➜fail]
          (when (¬ tape)
            )



           ((∧ tape (typep tape 'vector))
            (setf (head tm) tape)
            (setf (tape tm) tape)
            [➜ok tm]
            )
          (t
            (call-next-method keyed-parms ➜) ; pick up tape-machine's init for non consp tapes
            ))
        )))
