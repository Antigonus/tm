#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

 SBCL would not recognize these defclass declarations when in the 
 same file.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (defclass tape-machine ()
    (
      (HA ; locates a cell on the tape
        :initarg :HA 
        :accessor HA
        )
      (tape ; a sequence of cells, each that may hold an object
        :initarg :tape
        :accessor tape
        )
      ))

