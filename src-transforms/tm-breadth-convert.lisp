#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  This machine's tape is woven in a depth first pattern through
  the base tm interpreted as a tree.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; making other objects from tm-list machines
;;
  ;; This returns list of the current subspace that the machine is in.
  ;; To convert the entire space, apply to-list to the original base tm.
  (defmethod to-list ((tm tm-breadth)) (to-list (tape tm)))
  
