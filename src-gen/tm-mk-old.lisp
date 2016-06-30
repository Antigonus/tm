#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

 SBCL would not recognize these defclass declarations when in the 
 same file.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a tape machine
;;
;;  Entanglments is a list of machines that share the tape.  This is one list that is
;;  shared by all entangle machines.  Hence, a machine will be entangled with itself.
;;
  (defclass tape-machine (nd-tape-machine)
    (
      (entanglements ; list of other tape-machines that share the same tape
        :initarg entanglements
        :accessor entanglements
        )
      ))

