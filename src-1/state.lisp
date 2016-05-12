#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

A parked head acts like a head on a padding cell.  Actions that occur to the 
right of the cell are all valid.  However, actions that would occur on the 
cell are not.

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; head parking - moving the head into and out of the address space
;;
  (defun unmounted (tm)
    (∨
      (typep tm 'tm-void)
      (typep tm 'tm-parked)
      ))

  ;; this is the more common call for synch
  (defun mounted (tm)
    (¬ (unmounted tm))
    )
