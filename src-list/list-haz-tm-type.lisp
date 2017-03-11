#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  The haz machine is both a solo and an nd machine combined.


|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; a specialization
;;
  (def-type list-haz-tm (haz-tape-machine list-solo-tm list-nd-tm)())

    
