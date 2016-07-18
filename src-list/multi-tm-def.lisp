#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  yes destrictuve operations.
  yes copy operations.

  Introduces 'entanglement accounting' to facilitate both destructive operations and
  multiple machine son the same tape. Entanglement accounting includes both collision
  detection between entangled machines, and tape reference updates.

  The generic interface is extended by the introduction of functions that both perform
  destructive operations, and require the use of entangled copy operations.
  (At the time of this writing there are no such functions.)  However, the implementations
  of a number of functions are modified.

  There is no ambiguity in the diamond inheritance at the time of this writing, because
  the nd-tape-machine extentions and the solo-tape-machine extentions are mutually
  exclusive.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a tape machine
;;
  (defclass multi-tape-machine (nd-tape-machine solo-tape-machine)
    (
      (entanglements ; list of tape-machines that share the tape
        :initarg entanglements
        :accessor entanglements
        )
      ))


