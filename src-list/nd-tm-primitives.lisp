#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

non-destructive operation primitives

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; copying
;;  
  ;; wonder if this just should be a keyword option for init
  (defgeneric init-entangled (tm1 tm-orig)) ; tm1 and tm-orig will have the same type


;;--------------------------------------------------------------------------------
;; head location
;;
;;  having two machines to compare means that we must have made a copy at some point
;;
  (defgeneric heads-on-same-cell
    (
      tm0
      tm1 
      &optional
      cont-true
      cont-false
      &rest ⋯
      ))
    
