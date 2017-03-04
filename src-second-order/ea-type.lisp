#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Manager synchronizes operation on tape machines.

|#

(in-package #:tm)

(def-type entanglment-descriptor ()
  (
    entanglement-lock
    counter-lock
    counter-condition
    entanglements
    ))
  
(defun use-entanglments ( (ed entanglement-descriptor) work)
  )

(defun use-head-motion ( (ed entanglement-descriptor) work)
  )


(def-type ea-tm (status-tm)
  (
    (entanglements ; an solo list of machines in this entanglement group
      :initarg :ed
      :accessor ed
      )
    ))

(def-type abandoned-ea (ea-tm))
(def-type active-ea (ea-tm))
(def-type empty-ea (ea-tm))
(def-type parked-ea (ea-tm))
