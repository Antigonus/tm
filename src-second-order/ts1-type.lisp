#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  A thread safe version of the ea machine.

  Uses algorithm 1 to achieve thread safety (see
  ../docs/implementation/multiple-threads.txt).  In algorithm 1 there is one lock for all
  head motion and access to entanglement instances.  The assumption behind algorithm 1 is
  that: head motion is so quick that a more complex algorithm which faciliated more
  parallelism would actually be slower, due to the extra overhead.

  ea-tm already acquires the entanglements lock before operating on the entanglements.

  We keep the entangelements list so as to know:
   1. which machines to make empty, or not empty
   2. which machines to update their leftmost cell pointer for a◧  or d◧
   3. which machines to check for a collision before deleteing a cell
  
  Each of these operations will get a lock on the entanglements list while
  using it.

  With multiple threads we must also consider:
   1. head moves for one entangled machine, while the other entangled machine
      is deleting the very cell being moved to.
   2. cue-leftmost just as a cell is removed from leftmost.  (adding a cell
      to leftmost is a race, but it does not affect the integrity of the 
      machine.  Our locks are about machine integrity, races are the programmer's
      problem.)
   3. address incf gets the wrong answer, because between the read and write of
      the incremented address, the address value was written.

  To prevent such hazards, before any head motion we acquire the resource ownership
  token.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-type ts1-tm (ea-tm)())

  (def-type ts1-abandoned (ts1-tm status-abandoned)())
  (def-type ts1-active    (ts1-tm status-active)())
  (def-type ts1-empty     (ts1-tm status-empty)())
  (def-type ts1-parked    (ts1-tm status-parked)())

;;--------------------------------------------------------------------------------
;; state transition functions
;;
  (defun-typed to-abandoned ((tm ts1-tm)) (change-class tm 'ts1-abandoned))
  (defun-typed to-active    ((tm ts1-tm)) (change-class tm 'ts1-active))
  (defun-typed to-empty     ((tm ts1-tm)) (change-class tm 'ts1-empty))
  (defun-typed to-parked    ((tm ts1-tm)) (change-class tm 'ts1-parked))



