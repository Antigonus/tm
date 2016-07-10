#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  This is the tape machine model shown to the public.  It provides both destructive
  and general destructive operations.  In the latter case it assures machine integrity
  through entanglement accounting.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a tape machine
;;
;;  entanglments is a list of machines that share the tape.  Note, normally a machine is
;;  entangled with itself.  If it happens that a machine is not entangled with itself,
;;  some functions, such as ∀-parked, might return non-intutive results.
;;
  (defclass tape-machine (nd-tape-machine)
    (
      (entanglements ; list of other tape-machines that share the same tape
        :initarg entanglements
        :accessor entanglements
        )
      ))

