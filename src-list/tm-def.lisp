#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  The basic tape machine. 

  There are no entanglement operations, no destructive operations, and the machine viewed
  as a container is not stateful.

  Because there are no entanglement operations, functions that require moving the machine
  to do work, indeed move the machine.  In right going only machines, it is also not
  possible for a function to move the head, then move it back. Hence progress towards
  the end of a tape will be monotonic.

  Because the basic tape machine has no non-destructive operations, the programming
  paragidgm will be that of non-destructive programming.

  Because basic tape machines are not stateful, machines that exist will always have at
  least one member.  A function that is passed a container as an operand can not delete
  the container, hence basic tape machines never disappear through side effects.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (defclass tape-machine ()
    (
      (HA ; locates a cell on the tape
        :initarg :HA 
        :accessor HA
        )
      (tape ; a sequence of cells, each that may hold an object
        :initarg :tape
        :accessor tape
        )
      ))

