#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  The basic tape machine. 

  There are no entanglement operations, no destructive operations, and the machine viewed
  as a container has no status.

  Because basic tape machines have no non-destructive operations, the programming
  paragidgm when using them will be that of non-destructive programming, at least relative
  to the use of the machines.

  Because basic tape machines have no status, machines that exist will always have at
  least one member.  A function that is passed a container as an operand can not delete
  the container, hence basic tape machines never disappear through side effects.

  Because there are no entanglement operations, a function that is passed a basic tape
  machine as an operand, must step the head of that machine if it is to read any cell
  other than the one initially under the head.  Furhtermore, in right going only machines,
  it is not possible for a function to step the head, then step it back. Hence 1) progress
  towards the end of a tape will be monotonic. 2) the range of cells used by the funciton
  will be apparent.

|# 

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-type tape-machine ()())

;;--------------------------------------------------------------------------------
;; initialize a tape machine of the specified class to hold the specified instances
;;
  (def-function-class init (instance &optional init-value ➜))

  ;; Often this will be called via 'call-next-method.  The first cell must already exist
  ;; on the tape when this is called. That cell must have an instance of '∅'.
  (defun-typed init
    (
      (tm tape-machine)
      &optional 
      init-parms
      ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜fail (λ()(error 'bad-init-value)))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (destructuring-bind
        (&key tape tm &allow-other-keys) init-parms
        (cond
          ((∧ tape (typep tape 'sequence))
            (-s* tm)
            (w tm (elt tape 0))
            (loop
              for item in (subseq tape 1) do
              (as tm item {:➜ok #'do-nothing :➜no-alloc ➜no-alloc})
              )
            [➜ok tm]
            )
          ((∧ tape (typep tape 'tape-machine))
            (c-fit tape tm)
            [➜ok tm]
            )
          (tape [➜fail]) 
          (t ; there is no obligation to provide an initialization sequence
            [➜ok tm]) 
          ))))


  (defun mk (tm-class &optional init-parms ➜)
    (let(
          (instance (make-instance tm-class))
          )
      (init instance init-parms ➜)
      ))


