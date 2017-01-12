#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  No destrictuve operations.
  Yes, copy operations.

  Because there are no destructive operations it is safe to use machines
  that are entangled.  (Entangled machines have independent heads, but share
  a tape.)

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a tape machine
;;
  (defclass nd-tape-machine (tape-machine)())

