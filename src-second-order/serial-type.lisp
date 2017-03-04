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


(def-type mtm ()
  (
    (base ; the machine being managed
      :initarg :base
      :accessor base
      )
    (ed ; an entanglements descriptor
      :initarg :ed
      :accessor ed
      )
    ))

(def-type abandoned-mtm (mtm))
(def-type active-mtm (mtm))
(def-type empty-mtm (mtm))
(def-type parked-mtm (mtm))
