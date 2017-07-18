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

When the call interface of a tape function differes from its tape machine analog we append
<tape> or <cell> to its name.

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
  (def-function-class leftmost (tape &optional ➜)) ; returns a cell
  (defun-typed leftmost ((tape tape-empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))

  (def-function-class rightmost (tape &optional ➜)) ; returns a cell
  (defun-typed rightmost ((tape tape-empty) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))

  ;; accepts a :direction parameter, defaults to right going
  (def-function-class boundary-cell (tape &optional ➜)) ; returns a cell
  (defun-typed boundary-cell ((tape tape-empty) &optional ➜)
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
  ;; relative to leftmost
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
      (esr (leftmost tape) ➜)
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
      (esw (leftmost tape) instance ➜)
      )

  ;;----------------------------------------
  ;; relative to rightmost
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
      (esr (rightmost tape) ➜)
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
      (esw (rightmost tape) instance ➜)
      )

;;--------------------------------------------------------------------------------
;; topology manipulation
;;
  ;; prepends tape0 to tape1
  (def-function-class epa<tape> (tape1 tape0))
  (defun-typed epa<tape> ((tape1 tape) (tape0 tape-empty)) (do-nothing))

  ;; inserts the given cell as a new leftmost cell
  (def-function-class epa<cell> (tape cell))

  (def-function-class epa (tape instance &optional ➜)
    (:documentation
      "Entangle copy, Park head, Append. Allocates a new cell, initializes is to the given
      instance, and prepends it to the tape."  ))

  ;; for a doubly linked list, these are the 'operate on tail' versions of the above
  (def-function-class ◨a<cell> (tape cell))
  (def-function-class ◨a<instance> (tape instance))

  (def-function-class ◨a (tape instance &optional ➜)
    (:documentation
      "◨ (rightmost), append.  Allocates a new cell, initializes it to the given instance,
      and appends it to the tape."  ))
  ;; #'◨a on an empty machine is same as #'epa
  (defun-typed ◨a ((tm tape-machine-empty) instance &optional ➜)
    (epa tm instance ➜)
    )

  ;; removes the leftmost cell and returns it
  ;; (➜ok #'echo) (➜rightmost (be ∅))
  (def-function-class epd<cell> (tape &optional ➜))
  (defun-typed epd<cell> ((tape tape-empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜rightmost (λ()(error 'dealloc-on-rightmost)))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  ;; releases tape data
  ;; afterward tape will be empty
  (def-function-class epd+<cell> (tape &optional ➜))
  (defun-typed epd+<cell> ((tape tape-empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜rightmost (λ()(error 'dealloc-on-rightmost)))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  ;; deletes rightmost from the tape and returns it
  ;; (➜ok #'echo) (➜leftmost (be ∅))
  (def-function-class ep-d<cell> (tape &optional ➜))
  (defun-typed ep-d<cell> ((tape tape-empty) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜leftmost (λ()(error 'dealloc-on-leftmost)))
        &allow-other-keys
        )
      ➜
      [➜leftmost]
      ))

  ;; appears to delete the leftmost cell, but doesn't, hence avoiding sharing issues
  ;; however external references to the right neighbor of leftmost become orphaned
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
    (d.<cell> (leftmost tape) ➜)
    )

  ;; returns right neighbor cell
  ;; (➜ok #'echo) (➜rightmost (λ()(error 'dealloc-on-rightmost)))
  ;; cell must be on the tape
  (def-function-class d+<cell> (tape cell &optional ➜))
  (defun-typed d+<cell> ((tape tape-empty) cell &optional ➜)
    (destructuring-bind
      (&key
        (➜rightmost (λ()(error 'dealloc-on-rightmost)))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
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
  (defun-typed length-is-one ((tape tape-active) &optional ➜)
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
  (defun-typed length-is-two ((tape tape-active) &optional ➜)
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

 (def-function-class maximum-address (tape &optional ➜))
 (defun-typed maximum-address ((tape tape-empty) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
 (defun-typed maximum-address ((tape tape-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let(index cell)
        (labels(
                 (init-1 (cell)
                   (right-neighbor cell
                     {
                       :➜ok
                       (λ(the-right-neighbor)
                         (incf index)
                         (init-1 the-right-neighbor)
                         )
                       :➜rightmost
                       #'do-nothing
                       }))
                 (init-0 ()
                   (setf index 0)
                   (setf cell (leftmost tape))
                   (init-1 cell)
                   )
                 )
          (init-0)
          )
        [➜ok index]
        )))
                
     

     
     
