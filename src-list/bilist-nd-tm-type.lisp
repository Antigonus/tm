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
  (def-type bilist-nd-tm (list-nd-tm nd-tape-machine list-tm)())

    
;;--------------------------------------------------------------------------------
;; make a tm-entangled machine that shares the tape with tm-orig
;;
  ;; inherit entangle from list-nd-tm
