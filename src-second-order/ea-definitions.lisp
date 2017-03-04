#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; park all machines in the entanglement group
;; this is a 'private' function used by functions that add a cell to an enpty machine
;;
  ;; an empty machine is already parked
  (defun park-all (tm)
     ...

        (change-class tm 'ea-parked)
      )


;;--------------------------------------------------------------------------------
;; make all machine active
;; used by functions that add a cell to an empty machine then step into it
;;
  ;; an empty machine is already parked
  (defun-typed active-all ((tm status-empty) &optional ➜)
..
         (change-class tm 'ea-active)
      )



