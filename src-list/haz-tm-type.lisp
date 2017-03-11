#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  This machine has hazards.  It is possible to entangle machines, and then have one
  machine delete the cell the other machine has a head on.  It is safe to use this
  machine with a second-order wrapper, which manages the hazards.  E.g. with the
  ea-type.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-type haz-tape-machine (solo-tape-machine nd-tape-machine)())

