#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make nd-tape-machine lists.


|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; a specialization
;;
  (def-type list-nd-tm (nd-tape-machine list-tm)())

    
