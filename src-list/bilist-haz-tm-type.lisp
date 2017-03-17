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
  (def-type bilist-haz-tm (haz-tape-machine bilist-solo-tm bilist-nd-tm)())

    
