#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Debug printing

  
|#
  (in-package #:tm)

;;------------------------------------------------------------------------------------------
;; debug print synchronizer
;;
;; Synchronizes debug print messages in a multithreaded environment, so that they will not
;; overlap. The print occurs within a function that is passed in.
;;
;;   (prins (print "hello") (print "goodbye"))
;;

  (defvar *print-turned-on* t)
  (defvar *print-lock* (bt:make-lock))

  (defun turn-print-off () (setf *print-turned-on* ∅))
  (defun turn-print-on () (setf *print-turned-on* t))

  (defmacro prins (&body f)
    `(bt:with-lock-held (*print-lock*)
       (when *print-turned-on*
         ,@f
         (finish-output nil)
         )))
