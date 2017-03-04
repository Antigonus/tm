#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  A second order structure to add status to a machine.

  First order machines only exist when there is something to put in them.  Here I
  introduce a second order concept of emptiness.  A first order machine becomes empty when
  it is first parked, and then the last cell is requested to be deleted.  When this
  happens, we change-class the second order machine to be the status-empty machine, and
  the machine will be functionally empty from the point of view of the interface.
  However, the base first order machine will still be there, and will still have one cell.
  For sake of memory efficiency, we invoke garbage collection for the instance held in
  this one cell by writing nil to it.  Consequently, as an artificial constraint, all
  first order machines must be capable of having a single cell that allows one to write
  nil to it.  This might be a challenge for some abstract machines.

  I had also considered, and have in the past implemented, nulling out the base slot when
  the base machine becomes empty, but then when a new cell is added we must recreate the
  base machine, which incurs overhead and requires knowing its type and initialization
  parameters.  Knowing the base machine's type is rather simple, as we can store it in a
  slot, but the recreation overhead is unknown, and it might not be possible to know how to
  correctly initialize the recreated machine. I suppose we could add a 'this is how to
  recreate me' command.  Anyway, the current implementation voids all that complexity
  by just keeping the base machine around with a single cell (that as the last action we
  wrote nil to). 


|#

(in-package #:tm)

(def-type status-tm (identity-tr)
  (
    (base ; the machine being managed
      :initarg :base
      :accessor base
      )
    ))

(def-type status-abandoned (status-tm)())
(def-type status-active    (status-tm)())
(def-type status-empty     (status-tm)())
(def-type status-parked    (status-tm)())
