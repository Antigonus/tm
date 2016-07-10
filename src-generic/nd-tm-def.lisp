#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  This machine does not implement any destructive operations.  (See
  docs/concept/tape-sharing-hazards.txt).  Hence we can define cue-to and mk-cue-to for
  nd-tape-machines.


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a tape machine
;;
  (defclass nd-tape-machine (tape-machine)())

