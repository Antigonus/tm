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

  (def-function-class with-entangled (tm continuation))

;;--------------------------------------------------------------------------------
;; stepping with a boundary, boundaries are inclusive
;;
  ;; although we don't make any copies in this function, we do have two tape
  ;; machines that are on the same tape.  That can not happen with a solo machine
  ;; so nd-tape-machine is as far up the inheritance tree that this can go.
  (def-function-class s≠ (tm0 tm1 &optional ➜)
    (:documentation
      "tm0 and tm1 are on the same tape. 
       If tm0's head is on the same call as tm1's head, take cont-bound.  Otherwise
       if tm0's head is on the rightmost cell, take cont-rightmost.  Otherwise,
       step tm0.
      "
      ))

;;--------------------------------------------------------------------------------
;; head location comparison
;;
;; head location comparison only makes sense if we have two heads for which to make a
;; comparison. nd machines indeed can have more than one head, so ..
;;
  (def-function-class heads-on-same-cell (tm0 tm1 &optional ➜))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  
  (def-function-class ◨a (tm instance &optional ➜)
    (:documentation
      "Allocates a cell to the right of rightmost (thus becoming the new rightmost)."
      ))



  


