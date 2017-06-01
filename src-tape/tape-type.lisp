#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Tape has its own interface independent of that of tape machines.  Tape
machines can make use of this interface when it is useful to them.
  
There is no head over a tape, so the tape machine interface does not
take into account entanglements, collisions, or the like.

|# 

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-type tape ()())



