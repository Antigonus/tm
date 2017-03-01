#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  central clearing house for events

|#

(in-package #:tm)

(def-type event-station ()
  (
    (events-in-waiting ; hash table of events
      :initform (make-hash-table)
      )
    (lambdas-in-waiting ; hash table of lambdas
      :initform (make-hash-table)
      )
    ))

(defun register (event)
  (cond
    ((symbolp event) 
      (setf (gethash event))
      )
    ((symbolp (car event))
      (setf (gethash (car event)) (cdr (event)))
      )
    (t 
       
      
  (if (symbolp event)
    (setf (gethash event) nil)
    (if (symbo
    ))




