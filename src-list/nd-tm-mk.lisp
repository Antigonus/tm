#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; make an entangled copy of another tape machine
;;
  (def-function-class entangle (tm-orig &optional ➜))
