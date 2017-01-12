#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list machines.

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass list-tm (tape-machine)
    (
      (head ; locates a cell on the tape
        :initarg :head 
        :accessor head
        )
      (tape ; a sequence of cells, each that may hold an instance
        :initarg :tape
        :accessor tape
        )
      ))

