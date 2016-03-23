#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list machines from other objects.
  Make other objects from list machines.

|#

(in-package #:tm)



;;--------------------------------------------------------------------------------
;; making other objects from tm-list machines
;;
  (defmethod to-sequence ((tm tm-list))(tape tm))
  (defmethod to-list ((tm tm-list))(tape tm))
  
