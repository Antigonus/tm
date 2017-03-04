#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Manager synchronizes operation on tape machines.

|#

(in-package #:tm)

(def-type ea-tm (status-tm)
  (
    (entanglements ; an solo list of machines in this entanglement group
      :initarg :entanglements
      :accessor entanglements
      )
    ))

(def-type abandoned-ea (status-abandoned))
(def-type active-ea (status-active))
(def-type empty-ea (status-empty))
(def-type parked-ea (status-parked))
