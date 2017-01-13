#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make ea-tape-machine lists.

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; a specialization
;;
  (def-type list-ea-tm (ea-tape-machine list-nd-tm list-solo-tm)())

    
