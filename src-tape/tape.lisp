#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Architectural definition of a tape.

This tape machine interface does not take into account entanglements or threads.  These
things must be enforced externally.

Though these are tapes, not tape machines, function names found here appear to refer to
head operations.  For example, 'e-s*' which reads as entanglement copy and step left to
leftmost.  These names are used only because they are descriptive of what the
corresponding function does to the tape.

Not all tapes implement all of the interface.  The tapes only implement the interface
portions that are primitive relative to the implementation.  As an example, the
singly linked list has no left going operations.

The command #'leftmost will always return a cell. We do not implement the overhead
of dealing with the status of the tape within the tape.

When the call interface of a tape function differes from its tape machine analog
we append <tape> or <cell> to its name.

|# 

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-type cell ()()) ; a union of cell types
  (def-type tape ()()) ; a union of tape types

;;--------------------------------------------------------------------------------
;; accessing instances
;;
  (def-function-class e-s*r<tape> (tape))

  ;; (➜ok #'echo) (➜rightmost (be ∅))
  (def-function-class e-s*sr (tape &optional ➜))

  (def-function-class e-s*w<tape> (tape instance))

  ;; (➜ok #'echo) (➜rightmost (be ∅))
  (def-function-class e-s*sw (tape instance &optional ➜))

  ;; for doubly linked lists we also have:
  (def-function-class es*r<tape> (tape))
  (def-function-class es*-sr (tape &optional ➜))

  (def-function-class es*w<tape> (tape instance))
  (def-function-class es*-sw (tape instance &optional ➜))

;;--------------------------------------------------------------------------------
;; topology queries
;;
  (def-function-class =<cell> (cell-0 cell-1))
  (defun-typed =<cell> ((cell-0 cell) (cell-1 cell))
    (eql (cell cell-0) (cell cell-1))
    )

  (def-function-class r<cell> (cell)) ; returns an instance
  (def-function-class w<cell> (cell instance))

  (def-function-class leftmost (tape)) ; returns a cell

  ;; (➜ok #'echo) (➜rightmost (be ∅))
  (def-funciton-class right-neighbor (cell &optional ➜))

  ;; for doubly linked lists we also have:
  (def-funciton-class rightmost (tape)) ; returns a cell
  ;; (➜ok #'echo) (➜leftmost (be ∅))
  (def-function-class left-neighbor (cell &optional ➜))

;;--------------------------------------------------------------------------------
;; topology manipulation
;;
  ;; inserts the given cell as a new leftmost cell
  (def-function-class epa<cell> (tape cell)) 

  ;; makes and inserts a new leftmost cell initialized to the given instance
  (def-function-class epa<instance> (tape instance))

  ;; removes the leftmost cell and returns it
  ;; (➜ok #'echo) (➜rightmost (be ∅))
  (def-function-class epd<cell> (tape instance &optional ➜))

  ;; for a doubly linked list, these are the 'operate on tail' versions of the above
  (def-function-class es*a<cell> (tape cell))
  (def-function-class es*a<instance> (tape instance))
  ;; (➜ok #'echo) (➜leftmost (be ∅))
  (def-function-class es*-sd<cell> (tape instance &optional ➜))

  ;; given a cell removes its right neighbor and returns it
  ;; (➜ok #'echo) (➜rightmost (be ∅))
  (def-function-class d<cell> (cell &optional ➜))

  ;; left neighbor version for doubly linked lists
  ;; (➜ok #'echo) (➜leftmost (be ∅))
  (def-function-class -d<cell> (cell &optional ➜))

  ;; swaps instances with the rightneighbor, then deletes it
  ;; creates the appearence of deleting this cell even for a singly linked list
  ;; this is not needed for doubly linked lists, which can simply use s-d 
  ;; (➜ok #'echo) (➜rightmost (be ∅))
  (def-function-class d.<cell> (cell &optional ➜))

  ;; appears to delete the leftmost cell, but doesn't, hence avoiding sharing issues
  ;; however external references to the right neighbor of leftmost become orphaned
  ;; (➜ok #'echo) (➜rightmost (be ∅))
  (def-function-class e-s*d.<cell> (cell &optional ➜))

;;--------------------------------------------------------------------------------
;; length-tape
;;
  (def-function-class tape-length-is-one (tape &optional ➜))
  (defun-typed tape-length-is-one ((tape tape) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (right-neighbor (leftmost tape)
        {
          :➜ok (λ(cell)(declare (ignore cell))[➜∅])
          :➜rightmost ➜t
          })))

  (def-function-class tape-length-is-two (tape &optional ➜))
  (defun-typed tape-length-is-two ((tape tape) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (right-neighbor (leftmost tape)
        {
          :➜ok (λ(cell)
                 (right-neighbor cell
                   {
                     :➜ok (λ(cell)(declare (ignore cell))[➜∅])
                     :➜rightmost ➜t
                     }))
          :➜rightmost ➜∅
          })))


