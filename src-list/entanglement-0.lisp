#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Two or more machines are said to be entangled when they share the same tape.
Two or more machines are said to be in a collision when they have their heads
on the same cell. Only entangled machines can collide.

The purpose of entanglement accounting is to preserve the structural integrity
of tape machines in the presence of deallocation and state transitions.

Deallocation

  The destructive operation of deallocate region without spill can only occur when no
  other machine has its head on the deallocation region. The only candidates for such
  collisions are the entangled machines.

  It follows from our definitions that when a region is reallocated, via deallocate with
  spill, say from tape A to tape B, the machines entangled on tape A with their heads
  located on the reallocation region, then instead become machines entangled on tape
  B. However, we currently don't implement this.  Currently we instead take a 'not clear
  region' continuation if a region to be deallocated has any machine heads on it, 
  independent if the region is to be spilled or not.

Copy
  When a machine is copied a new entry is made on the entanglements list for the
  copy. The user may remove the copy from the entanglements list any time after it is
  no longer used.

Parked

  There is no head on the tape for a parked machine, hence it will never have a head on a
  region, and hence it will never trip a region not clear continuation.  However, the
  parked head does indicate a region starting from leftmost on the tape.  We might, for
  example call deallocate region, #'d, while passing it a parked machine to indicate the
  region to be deallocated, while expecting the leftmost cell to be deallocated.

  When a machine is parked, we keep the same instance as was used for the machine before
  it was parked, and instead modify the slot values.  In this manner the entanglements
  list reference remain correct.  We change-class on the instance so that class will 
  call the parked type functions.

  Parking a machine does not affect the entangled machines.

Void

  Only a singleton machine can become void.  (#'d* is considered a repeat of #'d).
  All machines in an entanglment group for a singleton machine are either parked,
  or have their head on the singleton cell.

  If any machine in the entanglement group has its head on the singleton cell then it can
  not be deleted, and thus the system can not collapse to void. In the converse, if
  the system is collapsing to void, then no other machine has its head on the singleton
  cell.

  When it is possible to transition to void, and one of a group of entangled machines
  indeed does transition to tm-void, then we must go through the entanglement list and
  transition all the machines to void. In the inverse operation when a cell is added
  causing a transition away from void, then all the entangled machines must also have
  their tapes updated and transition away from void.

  It is because of these transitions that we have been forced to add the entanglement
  feature (I was fine with leaving deallocation collision accounting to the user ;-).  We
  did not need to do these updates in the earlier C++ implementation, as we dispatched
  functions directly from a sharted state variable rather than from the type of the
  instance.


Region

  A region machine can not step beyond the leftmost or rightmost of the region, nor affect
  any cell outside the region.  Hence, given a region machine it is not possible to 
  affect or detect the non-region portion of the space.

  Suppose we have two machines, machine A which is in a given space, and machine B which
  is in a region of that space.  Now machine A has its head on a cell that is part of the
  region.  This is perfectly legal for machine A as machine A has purview of the entire
  space.  (For a hiearchy of spaces instead, use subspaces instead of regions.)

  Now suppose that machine B wants to deallocate the very same cell.  This may be a normal
  destructive operation on the region.  If we allowed this deallocation, then machine A
  would become malformed, so we can not allow it.  (Hence it is possible in certain cases
  for a region machine to detect the presence of machines defined on the larger space,
  while still not detect the non-region portions of the space.)

  The region location is a machine that belongs to the larger space, and not to the
  region. Hence the entanglement issues for it are as for any other machines in the
  space.  The same holds for the rightmost, and the head position machines as well.
  Hence, much of the entanglement problem for regions will be taken care of without
  any special consideration.

  The region itself is a tape machine.  When we copy it, an entry will be added to the
  entanglement list.  This information will be used should the region transition to or
  from void.  The region struct is shared, so the copy does not affect this.  The head
  value is a tape machine, so copy would just make another reference to the same machine.
  It is this case that caused us to move copy from a function to a dispatched method.

Copy and entanglement

  It is common for a routine to create a copy of a tape machine within a lexical scope so
  as not to perturb the state of the machine passed in.  This is typically done by calling
  copy.  The copy machine is then entangled.

  When the copy machine reaches the end of scope it is released for garbage collection.  
  Hence, it initially seemed like a good way to handle copy entanglements was to hold
  them with weak pointers.  The problem with that approach is that we don't know when
  the garbage collector will get around to releasing it.  Calling the garbage collector
  to force the release is too expensive.  (And in our tests using a portable weak-pointer
  library, even after calling gc the entanglments weak pointer still pointed at the
  copy machine.  This is confusing, but in any case gc is not a good answer).

  Hence, we are forced to adopt the discipline of manually disentangling copy machines
  before they leave scope.  Currently we use disentangle to do this, though it
  would be better to provide a macro form analogous to let, that does the copy and
  the cleanup, even in the presence of exceptions.
  

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; detecting a collision
;;
  (defun collision (tm0 tm1 &optional (cont-true (be t)) (cont-false (be ∅)))
    "tm0 and tm1 are distinct machines, and have their heads on the same cell."
    (if
      (∧
        (¬ (eq tm0 tm1))
        (heads-on-same-cell tm0 tm1)
        )
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defun ∃-collision (tm &optional (cont-true (be t)) (cont-false (be ∅)))
    "tm0 collides with an entangled machine."
    (let(
          (es (entanglements tm))
          )
      (unless es (return-from ∃-collision (funcall cont-false)))
      (cue-leftmost es)
      (∃ es (λ(e)(collision tm (r e)))
        cont-true
        cont-false
        )))

  (defun ∃-collision◧ (tm &optional (cont-true (be t)) (cont-false (be ∅)))
    "an entangled machine is on leftmost."
    (let(
          (es (entanglements tm))
          )
      (unless es (return-from ∃-collision◧ (funcall cont-false)))
      (cue-leftmost es)
      (∃ es (λ(e)(on-leftmost (r e)))
        cont-true
        cont-false
        )))


