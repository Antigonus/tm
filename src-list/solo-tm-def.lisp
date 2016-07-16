#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  yes destrictuve operations.
  no copy operations.

  Because solo machines can not be copied, they are never entangled with other machines.
  Hence, it is safe to perform destructive operations on solo machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a tape machine
;;
  (defclass solo-tape-machine (tape-machine)())

