#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

'ea' - entanglement accounting

We allow entanglement of machines, and delete commands.  However, if one attempts
to delete the cell that an entangled machine has its head on, then one takes a
collision continuation.  We use 'entanglement accounting' to keep track of which
machines are entangled.

place holder .. we haven't written this yet.

|#

;;--------------------------------------------------------------------------------
;; type
;;
  (def-type tm-ea (tm)())
