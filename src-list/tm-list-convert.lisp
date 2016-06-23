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
  (defmethod unmount-0 ((tm tm-list) (state void)) ∅)
  (defmethod unmount-0 ((tm tm-list) (state parked)) (tape tm))
  (defmethod unmount-0 ((tm tm-list) (state active)) (tape tm))
  
