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

  (defvar *prins-on* ∅)
  (defvar *prins-lock* (bt:make-lock))

  (defun turn-on-prins () (setf *prins-on* t))
  (defun turn-off-prins () (setf *prins-on* ∅))

  (defmacro prins (&body f)
    `(bt:with-lock-held (*prins-lock*)
       (when *prins-on*
         ,@f
         (finish-output nil)
         )
       t ; so print can be embedded in conjunctive phrases
       ))

