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
the name a <tape> suffix if it operates on a whole tape,  or a <cell> suffix if it
operates on a cell and a tape.  (If an operation just operated on cells it would be
in src-cell/interface.)

We push end cases into dispatch.  Dispatch already makes decisions based on operand type,
so it won't mind having a few more types.

|# 

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type
;;
  (def-type tape ()()) ; a union of tape types

  (def-type tape-abandoned (tape)()) 
  (def-type tape-valid (tape)())

  (def-type tape-active (tape-valid)()) 
  (def-type tape-empty (tape-valid)())

  (def-function-class to-abandoned (tape))
  (def-function-class to-empty     (tape))
  (def-function-class to-active    (tape))

;;--------------------------------------------------------------------------------
;; tape queries
;;
  (def-function-class left-bound (tape &optional ➜)) ; returns a cell
  (defun-typed left-bound ((tape tape-empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))

  (def-function-class right-bound (tape &optional ➜)) ; returns a cell
  (defun-typed right-bound ((tape tape-empty) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))

  ;; accepts a :d (direction) parameter, defaults to direction 0
  (def-function-class boundary (tape &optional ➜)) ; returns a cell
  (defun-typed boundary ((tape tape-empty) &optional ➜)
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
  ;; relative to left-bound
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
      (r (left-bound tape) ➜)
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
      (w (left-bound tape) instance ➜)
      )

  ;;----------------------------------------
  ;; relative to right-bound
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
      (r (right-bound tape) ➜)
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
      (w (right-bound tape) instance ➜)
      )

;;--------------------------------------------------------------------------------
;; topology manipulation
;;
  ;; prepends tape0 to tape1
  (def-function-class epa<tape> (tape1 tape0))
  (defun-typed epa<tape> ((tape1 tape) (tape0 tape-empty)) t)

  ;; inserts the given cell as a new left-bound cell
  (def-function-class epa<cell> (tape cell))

  (def-function-class epa (tape instance &optional ➜)
    (:documentation
      "Entangle copy, Park head, Append. Allocates a new cell, initializes is to the given
      instance, and prepends it to the tape."  ))

  ;; appends a cell to right-bound 
  (def-function-class ◨a<cell> (tape cell))

  (def-function-class ◨a (tape instance &optional ➜)
    (:documentation
      "◨ (right-bound), Append.  Allocates a new cell, initializes it to the given instance,
      and appends it to the tape."  ))
  ;; #'◨a on an empty machine is same as #'epa
  (defun-typed ◨a ((tm tape-machine-empty) instance &optional ➜)
    (epa tm instance ➜)
    )

  ;; removes the left-bound cell and returns it
  ;; (➜ok #'echo) (➜right-bound (be ∅))
  (def-function-class epd<cell> (tape &optional ➜))
  (defun-typed epd<cell> ((tape tape-empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜right-bound (λ()(error 'dealloc-on-right-bound)))
        &allow-other-keys
        )
      ➜
      [➜right-bound]
      ))

  ;; releases tape data
  ;; afterward tape will be empty
  (def-function-class epd+<cell> (tape &optional ➜))
  (defun-typed epd+<cell> ((tape tape-empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜right-bound (λ()(error 'dealloc-on-right-bound)))
        &allow-other-keys
        )
      ➜
      [➜right-bound]
      ))

  ;; deletes right-bound from the tape and returns it
  ;; (➜ok #'echo) (➜left-bound (be ∅))
  ;; not available on a singly linked list
  (def-function-class ep-d<cell> (tape &optional ➜))
  (defun-typed ep-d<cell> ((tape tape-empty) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜left-bound (λ()(error 'dealloc-on-left-bound)))
        &allow-other-keys
        )
      ➜
      [➜left-bound]
      ))

  ;; Swaps data with the right neighbor, and deletes the right neighbor.
  ;; Creates appearance of deleting 'this cell'.
  ;; Doesn't work when 'this cell' if the right-bound cell.
  ;; Messes up external references to this cell and the right neighbor cell.
  ;; Almost didn't include this in the library, bit is useful for deleting left-bound
  ;; without having to update the left-bound pointer in the header.
  ;; (➜ok #'echo) (➜empty #'accessed-empty)
  (def-function-class ◧d.<cell> (tape &optional ➜))
  (defun-typed ◧d.<cell> ((tape tape-empty) &optional ➜)
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
    (d.<cell> (left-bound tape) ➜)
    )

  ;; returns right neighbor cell
  ;; (➜ok #'echo) (➜right-bound (λ()(error 'dealloc-on-right-bound)))
  ;; cell must be on the tape
  (def-function-class d+<cell> (tape cell &optional ➜))
  (defun-typed d+<cell> ((tape tape-empty) cell &optional ➜)
    (destructuring-bind
      (&key
        (➜right-bound (λ()(error 'dealloc-on-right-bound)))
        &allow-other-keys
        )
      ➜
      [➜right-bound]
      ))

;;--------------------------------------------------------------------------------
;; length
;;
  (def-function-class length-is-one (tape &optional ➜))
  (defun-typed length-is-one ((tape tape-empty) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

  (def-function-class length-is-two (tape &optional ➜))
  (defun-typed length-is-two ((tape tape-empty) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

                
     

     
     
