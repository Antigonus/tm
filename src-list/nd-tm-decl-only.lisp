#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

non-destructive operation primitives

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; head location
;;
;;  having two machines to compare means that we must have made a copy at some point
;;
  (def-function-class heads-on-same-cell (tm0 tm1 &optional ➜))
    
