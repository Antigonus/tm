#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Architectural definition of a cell.

A cell has 


A 'bound' is a link that cannot be followed.   


Multiple universe contents have mulitple contents components indexed by a universe id.
There can also be multiple world link sets. The universe id is specified as a keyword
parm.  I put it in the continuations list, defaults to 0.

|# 

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; type
;;
  (def-type cell ()())

;;--------------------------------------------------------------------------------
;; 
;;
  (def-function-class r<contents> (cell &optional ➜)) ; returns contents of cell
  (def-function-class w<contents> (cell instance &optional ➜)) ; writes contents of cell
  (def-function-class a◨<contents> (cell instance)) ; append to contents as a list

  (def-function-class r<neighbor> (cell &optional ➜))
  (def-function-class w<neighbor> (cell0 cell1 &optional ➜))
  (def-function-class a<neighbor> (cell0 cell1)) ; append to list of neighbors


;;--------------------------------------------------------------------------------
;; multiple universe support
;;
  (def-function-class a-contents-universe (cell


  ;; uses context to return a cell among the multiplexed
  (def-function-class ur<cell> (context cell))
  (defun-typed ur<cell> (context (cell cell))
    (declare (ignore context))
    cell
    )
  (defun-typed ur<cell> (context (cell cell-multiplexed))
    (let(
          (cell (get-hash (multiplexed cell) (id context)))
          )
      cell
      ))

  ;; Adds an alternative to the right neighbor cell-multiplexed node.
  ;; If the right neighbor is not an multiplexed node, it turns it into one.
  ;; If the alternative already exists ...
  (def-function-class ua<cell> (context cell))
  (defun-typed ua<cell> (context (cell cell))
    (declare (ignore context))
    cell
    )
  (defun-typed ua<cell> (context (cell cell-multiplexed))
    (setf (get-hash (alternatives cell) (id context)) cell)
    )

  ;; Removes an alternative from the right neighbor cell-multiplexed node,
  ;; and returns it.
  (def-function-class ud<cell> (context cell))
