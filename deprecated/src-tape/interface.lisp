#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Architectural definition of a tape.

Conceptually a tape is an array of cells.

This tape interface does not take into account entanglements or threads.  These
things must be enforced externally.

Not all tapes implement all of the interface.  The tapes only implement the interface
portions that are primitive relative to the implementation.  As an example, the singly
linked list has no left going operations.

When the call interface of a tape function differes from its tape machine analog we give
the name a <tape> suffix.  (If an operation just operated on cells it would be
in src-cell/interface.)

We push end cases into dispatch.  Dispatch already makes decisions based on operand type,
so it won't mind having a few more types.

|# 

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type
;;
  ;; a tape is a referenced cell, that cell is either an empty-tape, solitary, or
  ;; bound-left.  If it is bound-left it has a right neighbor.


;;--------------------------------------------------------------------------------
;; tape queries
;;
  (defun bound-left (tape &optional ➜)
    (bound tape ➜)
    )
  (defun bound-right (tape &optional ➜)
    (bound tape {:d *right* (o ➜)})
    )
  ;; accepts a :d (direction) parameter, defaults to direction 0
  (def-function-class bound (tape &optional ➜)) ; returns a cell
  (defun-typed bound ((tape tape-empty) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))

;;--------------------------------------------------------------------------------
;; data access
;;

  ;;----------------------------------------
  ;; relative to bound-left
  ;;
    (def-function-class ◧r (tape &optional ➜))
    (defun-typed ◧r ((tape tape-empty) &optional ➜)
      (declare (ignore tape))
      (destructuring-bind
        (&key
          (➜empty #'accessed-empty)
          &allow-other-keys
          )
        ➜
        [➜empty]
        ))
    (defun-typed ◧r ((tape tape-active) &optional ➜)
      (r tape ➜)
      )

    (def-function-class ◧w (tape instance &optional ➜))
    (defun-typed ◧w ((tape tape-empty) instance &optional ➜)
      (declare (ignore instance))
      (destructuring-bind
        (&key
          (➜empty #'accessed-empty)
          &allow-other-keys
          )
        ➜
        [➜empty]
        ))
    (defun-typed ◧w ((tape tape-active) instance &optional ➜)
      (w tape instance ➜)
      )

  ;;----------------------------------------
  ;; relative to bound-right
  ;;
    (def-function-class ◨r (tape &optional ➜))
    (defun-typed ◨r ((tape tape-empty) &optional ➜)
      (declare (ignore tape))
      (destructuring-bind
        (&key
          (➜empty #'accessed-empty)
          &allow-other-keys
          )
        ➜
        [➜empty]
        ))
    (defun-typed ◨r ((tape tape-active) &optional ➜)
      (r (bound-right tape) ➜)
      )

    (def-function-class ◨w (tape instance &optional ➜))
    (defun-typed ◨w ((tape tape-empty) instance &optional ➜)
      (declare (ignore tape instance))
      (destructuring-bind
        (&key
          (➜empty #'accessed-empty)
          &allow-other-keys
          )
        ➜
        [➜empty]
        ))
    (defun-typed ◨w ((tape tape-active) instance &optional ➜)
      (w (bound-right tape) instance ➜)
      )

;;--------------------------------------------------------------------------------
;; topology manipulation
;;
  ;; inserts the given cell as a new bound-left cell
  (def-function-class epa<tape> (tape cell &optional ➜))

  ;; With an empty tape, we already have a cell so we toss the one we are given.
  ;; This is legal, i.e. we are under no obligation to use it, rather we are 
  ;; just trying to save an allocation call.
  (defun-typed epa<tape> ((c0 tape-empty) new-cell &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (w c0 (r new-cell))
      (to-solitary c0)
      [➜ok]
      ))

  (def-function-class epa (tape instance &optional ➜))
  (defun-typed epa ((c0 tape-empty) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (w c0 instance)
      (to-solitary c0)
      [➜ok]
      ))
  (defun-typed epa ((c0 cell) instance  &optional ➜)
    (let(
          (new-cell (clone<cell> c0))
          )
      (epa<tape> c0 new-cell ➜)
      ))

  ;; Private.
  ;; Removes the last cell from a tape, thus causing it to be empty.  However, an empty
  ;; tape has one cell, albeit an empty one. Delete is obliged to return a cell to support
  ;; spilling, so we make one up. Hopefully the optimizer removes his code when we are not
  ;; spilling, or if others call this without spilling, maybe it should leave it be so
 ;; that the cached version can be used.
  ;;
    (defun d-last (tape)
      (let(
            (copy (clone<cell> tape))
            )
        (w<cell> tape ∅)
        (to-empty tape)
        (to-cell copy)
        copy
        ))

  ;; Removes the bound-left and returns it, (or the right bound if d= *left*).  The
  ;; neighbor of the removed bounding cell becomes the new bound and the tape head is
  ;; adjusted accordingly. 
  ;; returns a cell (➜ok #'echo) (➜bound-right (be ∅))
  ;; 
    (def-function-class epd<tape> (tape &optional ➜))
    (defun-typed epd<tape> ((tape tape-empty) &optional ➜)
      (destructuring-bind
        (&key
          (➜bound-right (λ()(error 'dealloc-on-bound-right)))
          &allow-other-keys
          )
        ➜
        [➜bound-right]
        ))
    (defun-typed epd<tape> ((tape solitary) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          &allow-other-keys
          )
        ➜
        [➜ok (d-last tape)]
        ))
 
  ;; Deletes the whole tape.
  ;; Afterward tape will be empty.
  ;; Returns the bound-left that is still connected to its neighbor
  ;;
    (def-function-class epd+<tape> (tape &optional ➜))
    (defun-typed epd+<tape> ((tape tape-empty) &optional ➜)
      (destructuring-bind
        (&key
          (➜bound-right (λ()(error 'dealloc-on-bound-right)))
          &allow-other-keys
          )
        ➜
        [➜bound-right]
        ))
    (defun-typed epd+<tape> ((tape tape-active) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          &allow-other-keys
          )
        ➜
        (d+<cell> (bound-left tape)
          {
            :➜ok
            (λ(rhs)
              (epa<tape> rhs (d-last tape))
              [➜ok rhs]
              )
            :➜bound-right 
            (λ()
              [➜ok (d-last tape)]
              )
            })))
                 

  ;; Swaps data with the right neighbor, and deletes the right neighbor.
  ;; Creates appearance of deleting 'this cell'.
  ;; Doesn't work when 'this cell' if the bound-right cell.
  ;; Messes up external references to this cell and the right neighbor cell.
  ;; Almost didn't include this in the library, bit is useful for deleting bound-left
  ;; without having to update the bound-left pointer in the header.
  ;; (➜ok #'echo) (➜empty #'accessed-empty)
  ;; returns a cell, but operates correctly for a tape
  ;;
    (def-function-class ◧d.<tape> (tape &optional ➜))
    (defun-typed ◧d.<tape> ((tape tape-empty) &optional ➜)
      (declare (ignore tape))
      (destructuring-bind
        (&key
          (➜empty #'accessed-empty)
          &allow-other-keys
          )
        ➜
        [➜empty]
        ))
    (defun-typed ◧d.<tape> ((tape tape-active) &optional ➜)
      (d.<cell> (bound-left tape) ➜)
      )




     
     
