#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

 SBCL would not recognize these defclass declarations when in the 
 same file.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; each tape machine may be in exactly one state
;;
;;  'void' speaks to allocation, and means there are no cells.  'empty' speaks to data and
;;  means that an existing allocation has no data in it.
;;
;;  Typically we do not keep track of what is empty, but rather emulate allocation and
;;  keep track of void. For example, if we have a fixed array tape, then keeping track
;;  which cells are empty requires some sort of external structure, but the same external
;;  structure could be said to be keeping track of allocation, and allocation is a more
;;  general model, so we do that instead.
;;
;;  As a avoid machine has no tape, it can have no head position. When voiding a machine
;;  we typically set the tape to null, so that the tape structure will be deallocated.
;;  Similarly we typically nullify the head state.
;;
;;  Parked means that we do not have a head state.  Consequently, a parked machine can not
;;  collide with an entangled machine. A parked machine tape may be non-void. When parking
;;  a machine we nullify the head state.
;;
;;  A void machine is also parked machine, but we only signal the void state. When there
;;  is an issue of head state being accessed in a void machine, the void machine will
;;  follow cont-parked, just like a parked machine would.  When there is an issue of the
;;  tape being accessed independent of the head (for example #'d◧), we follow cont-void.
;;
  (defclass state ()())
  (defclass void (state)())
  (defclass parked (state)())
  (defclass active (state)())

;;--------------------------------------------------------------------------------
;; a tape machine
;;
;;  HA holds the head state.  cue-leftmost resets this HA without refering
;;  to its prior value.  Parked and empty machine nullify the HA slot.
;;
;;  tape is the stuff that has been added to the container.  This is set to ∅
;;  when the machine goes to tm-void. The tape is restored with the function a◧.
;;
;;  parameters holds characterizing information unique to the instance.  For example for
;;  our integer recurrance generator, it is a struct that holds the min value (start
;;  value), the max value, and the increment.
;;
;;  entanglments is a list of machines that share the tape.  Note, normally a machine is
;;  entangled with itself.  If it happens that a machine is not entangled with itself,
;;  some tests, such as ∀-parked, may return non-intutive results.
;;
  (defclass tape-machine ()
    (
      (state ; state of the machine, type matters not value, one of empty, parked, active
        :initarg :state
        :accessor state
        )
      (HA ; locates a cell on the tape
        :initarg :HA 
        :accessor HA
        )
      (tape ; a sequence of cells, each that may hold an object
        :initarg :tape
        :accessor tape
        )
      (parameters ; for such things as generator that need seed values
        :initarg :parameters
        :accessor parameters
        )
      (entanglements ; list of other tape-machines that share the same tape
        :initarg entanglements
        :accessor entanglements
        )
      ))

