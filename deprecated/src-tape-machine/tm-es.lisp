#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

'es' - entanglement safe

Entangling two machines is unsafe because it is possible to use one machine to delete the
cell the head is on in the other machine.  He we have removed such delete operations from
the lexicon, by overriding them to take an error continuation.  One might say that when
one only uses tm-es stil machines that one is performing 'non-destructive programming'.

place holder .. we haven't written this yet.

|#

;;--------------------------------------------------------------------------------
;; type
;;
  (def-type tm-es (tm)())
