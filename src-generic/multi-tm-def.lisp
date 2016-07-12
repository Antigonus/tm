#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  yes destrictuve operations.
  yes copy operations.

  Introduces 'entanglement accounting' to facilitate both destructive operations and
  multiple machine son the same tape. Entanglement accounting includes both collision
  detection between entangled machines, and tape reference updates.

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


