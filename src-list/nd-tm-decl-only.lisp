#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

non-destructive operation primitives

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; entanglement
;;
  ;; predicate tells if two machines are entangled
  ;; two machines should be entangled only if the programmer called one of the
  ;; library functions to create an entangled copy.  If the programmer broke
  ;; his/her contract with the library and somehow manually entangled machines,
  ;; perhaps in a peculiar way, this test might or might not return true.
  (def-function-class entangled (tm0 tm1 &optional ➜))

;;--------------------------------------------------------------------------------
;; head location comparison
;;
;; head location comparison only makes sense if we have two heads for which to make a
;; comparison. nd machines indeed can have more than one head, so ..
;;
  (def-function-class heads-on-same-cell (tm0 tm1 &optional ➜))


  
  


