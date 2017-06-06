#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Architectural definition of a tape.

There is no head over a tape, so the tape machine interface does not
take into account entanglements, collisions, or the like.

|# 

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-type tape ()())

;;--------------------------------------------------------------------------------
;; accessing instances
;;
  (def-function-class e-s*r (tape &optional ➜))
  (def-function-class e-s*sr (tape &optional ➜))

  (def-function-class e-s*w (tape instance &optional ➜))
  (def-function-class e-s*sw (tape instance &optional ➜))

  (def-function-class es*r (tape &optional ➜))
  (def-function-class es*-sr (tape &optional ➜))

  (def-function-class es*w (tape instance &optional ➜))
  (def-function-class es*-sw (tape instance &optional ➜))

;;--------------------------------------------------------------------------------
;; accessing cells
;;
  (def-function-class epa (tape instance &optional ➜))
  (def-function-class epd (tape instance &optional spill ➜))

  (def-function-class es*a (tape instance &optional ➜))
  (def-function-class es*-sd (tape instance &optional spill ➜))
  

;;--------------------------------------------------------------------------------
;; length-tape
;;
  (def-function-class tape-length-is-one (tape &optional ➜))
  (def-function-class tape-length-is-two (tape &optional ➜))




