#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make solo-tape-machine bilists.

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; a specialization
;;
  (def-type bilist-solo-tm (list-solo-tm bilist-tm solo-tape-machine)())

    
