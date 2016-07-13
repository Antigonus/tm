#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Boundary logic for layer one tape machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; Bounding Conditions
;;
;;  An 'abandoned' machine is one that will not be used anymore. Upon abandoning a
;;  machine we disentangle it and set its head address, tape, and parameters to ∅.
;;
;;  A 'void' machine has no tape, consequently it can have no head address. When voiding
;;  a machine we typically set the HA and tape to ∅, so that the referenced objects might
;;  be deallocated.  However, this is not a hard requirement.  Voidness is determined 
;;  soley by the state.
;;
;;  A 'parked' machine has in common with a void machine that it has no head address.
;;  However, unlike a void machine it does have a tape. When parking a machine we
;;  typically set the HA to ∅, so that any referenced objet might be deallocated.
;;
;;  When there is an issue of the head address being accessed in a void machine, the void
;;  machine will follow cont-parked, just like a parked machine would.  When there is an
;;  issue of the tape being accessed in a void machine, independent of the head address
;;  (for example #'d◧), we follow cont-void.
;;
;;  An active machine is one that has escaped the boundary case algebra.
;;
  (defclass state ()())

  (defclass void (state)())
  (defclass parked (state)())
  (defclass abandoned (state)())
  (defclass active (state)())

;;--------------------------------------------------------------------------------
;; non-destructive tape machine
;;
;;  HA holds the head address.  cue-leftmost resets this HA without refering
;;  to its prior value.  Parked and empty machines typically nullify the HA slot.
;;
;;  tape is the stuff that has been added to the container.  This is set to ∅
;;  when the machine goes to tm-void. The tape is restored with the function a◧.
;;
;;  parameters holds characterizing information unique to the instance.  For example for
;;  our integer recurrance generator, it is a struct that holds the min value (start
;;  value), the max value, and the increment.
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
      ))

