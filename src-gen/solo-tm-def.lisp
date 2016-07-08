#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

   A solo machine has exclusive use of its tape.  A machine that has exclusive use of its
   tape can not be entangled, so it follows that we can perform destructive operations.
   However we can not perform operations that cause machines to share tapes, i.e. 
   cue-to and mk-cue-to.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a tape machine
;;
  (defclass solo-tape-machine (tape-machine)())

