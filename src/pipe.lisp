#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

A pipe is not a tape machine.  Rather it connects an source tape machine
to an destination tape machine.

Each time the pipe is stepped, it writes a new value to the destination
machine, and leaves the destination machine on the written cell.  Thus
another program can read the newly written value from the destination
tape machine. If it is not desired to build a destination tape, rather
one just wants the value at each step, the destination machine can be
singular.

 In general no contracts are made about the source machines,
though specific pipes may make such contracts.

Each pipe does some work.

|#

(in-package #:tm)
