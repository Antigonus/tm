#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make solo-tape-machine lists.

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; a specialization
;;
  (def-type list-solo-tm (solo-tape-machine list-tm)())

    
