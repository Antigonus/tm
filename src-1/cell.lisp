#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Architectural definition of a cell.

A cell has contents.  These contents can be multiplexed.

A cell has any number of neighbors.  Each neighbor may be multiplexed.

When accessing a channel of a plex, and that channel is not present, a parent
table is consulted to find a parent channel.  If there is no parent channel,
then there we take a no-such-channel continuation.  Only channel 0 will
not have a parent.

A 'bound' is a link that cannot be followed.  Bound links have a value of ∅.

|# 

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; type
;;
  (def-type cell ()())

;;--------------------------------------------------------------------------------
;; 
;;
  (def-function-class r<contents> (cell &optional ➜)) ; returns contents of cell
  (def-function-class w<contents> (cell instance &optional ➜)) ; writes contents of cell
  (def-function-class a◨<contents> (cell instance &optional ➜)) ; append to contents as a list

  (def-function-class r<neighbor> (cell &optional ➜))
  (def-function-class w<neighbor> (cell0 cell1 &optional ➜))
  (def-function-class a◨<neighbor> (cell0 cell1 &optional ➜)) ; append to list of neighbors


