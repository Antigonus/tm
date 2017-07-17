#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Architectural definition of a tape.

The basic tape type code does not employ iterators (or recursive equivalents), rather this
task is reserved for the tape machine.  Consequently the base type does not have such
things as a shallow copy or initialization from a sequence.

I have gone back and added right-nieghbor-n and left-neighbor-n to cell (cell.lisp).  This
is because it is easy to add 'n' to an array index for the arrays so I didn't want to
leave it off the interface.  Turns out, this might be unnecessary, as we might just
implement the array tape machine at the tape machine level, because the array cells do not
have introspective knowledge about their neighbors. 

This tape interface does not take into account entanglements or threads.  These
things must be enforced externally.

Although these are tapes, not tape machines, function names found here appear to refer to
head operations. For example, '◧sr' which reads as entanglement copy, step left to
leftmost, step right, and read.  These names are used only because they are descriptive of
what the corresponding function does, not because there is a tape head.  In this case,
◧sr, reads the second instance on the tape.

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

;;--------------------------------------------------------------------------------
;; composed tape queries
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
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (r<cell> (leftmost tape))]
      ))

  ;; (➜ok #'echo) (➜rightmost (be ∅))
  (def-function-class ◧sr (tape &optional ➜))
  (defun-typed ◧sr ((tape tape-empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed ◧sr ((tape tape-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ()(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (let(
            (leftmost (leftmost tape))
            )
        (right-neighbor leftmost
          {
            :➜ok 
            (λ(the-right-neighbor)
              [➜ok (r<cell> the-right-neighbor)]
              )
            :➜rightmost ➜rightmost
            }))))

  (def-function-class ◧snr (tape n &optional ➜))
  (defun-typed ◧snr ((tape tape-empty) n &optional ➜)
    (declare (ignore n))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
         )
      ➜
      [➜empty]
      ))
  (defun-typed ◧snr ((tape tape-active) n &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ()(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (let(
            (leftmost (leftmost tape))
            )
        (right-neighbor-n leftmost n
          {
            :➜ok 
            (λ(the-right-neighbor)
              [➜ok (r<cell> the-right-neighbor)]
              )
            :➜rightmost ➜rightmost
            }))))


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
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (w<cell> (leftmost tape) instance)
      [➜ok]
      ))


  ;; (➜ok #'echo) (➜rightmost (be ∅))
  (def-function-class ◧sw (tape instance &optional ➜))
  (defun-typed ◧sw ((tape tape-empty) instance &optional ➜)
    (declare (ignore instance))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
         )
      ➜
      [➜empty]
      ))
  (defun-typed ◧sw ((tape tape-active) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (λ()(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (let(
            (leftmost (leftmost tape))
            )
        (right-neighbor leftmost
          {
            :➜ok 
            (λ(the-right-neighbor)
              (w<cell> the-right-neighbor instance)
              [➜ok]
              )
            :➜rightmost ➜rightmost
            }))))

  (def-function-class ◧snw (tape n instance &optional ➜))
  (defun-typed ◧snw ((tape tape-empty) n instance &optional ➜)
    (declare (ignore n instance))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
         )
      ➜
      [➜empty]
      ))
  (defun-typed ◧snw ((tape tape-active) n instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (let(
            (leftmost (leftmost tape))
            )
        (right-neighbor-n leftmost n
          {
            :➜ok 
            (λ(the-right-neighbor)
              (w<cell> the-right-neighbor instance)
              [➜ok]
              )
            :➜rightmost ➜rightmost
            }))))


  ;; for doubly linked lists we also have:
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
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (r<cell> (rightmost tape))]
      ))

  (def-function-class ◨-sr (tape &optional ➜))
  (defun-typed ◨-sr ((tape tape-empty) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed ◨-sr ((tape tape-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜leftmost (λ()(error 'step-from-leftmost)))
        &allow-other-keys
        )
      ➜
      (let(
            (rightmost (rightmost tape))
            )
        (left-neighbor rightmost
          {
            :➜ok 
            (λ(the-left-neighbor)
              [➜ok (r<cell> the-left-neighbor)]
              )
            :➜leftmost ➜leftmost
            }))))
  (def-function-class ◨snr (tape n &optional ➜))
  (defun-typed ◨snr ((tape tape-empty) n &optional ➜)
    (declare (ignore n))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
         )
      ➜
      [➜empty]
      ))
  (defun-typed ◨snr ((tape tape-active) n &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜leftmost (λ()(error 'step-from-leftmost)))
        &allow-other-keys
        )
      ➜
      (let(
            (leftmost (leftmost tape))
            )
        (left-neighbor-n leftmost (- n)
          {
            :➜ok 
            (λ(the-left-neighbor)
              [➜ok (r<cell> the-left-neighbor)]
              )
            :➜leftmost ➜leftmost
            }))))


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
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (w<cell> (rightmost tape) instance)]
      ))

  (def-function-class ◨-sw (tape instance &optional ➜))
  (defun-typed ◨-sw ((tape tape-empty) instance &optional ➜)
    (declare (ignore tape instance))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed ◨-sw ((tape tape-active) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜leftmost (λ()(error 'step-from-leftmost)))
        &allow-other-keys
        )
      ➜
      (let(
            (rightmost (rightmost tape))
            )
        (left-neighbor rightmost
          {
            :➜ok 
            (λ(the-left-neighbor)
              [➜ok (w<cell> the-left-neighbor instance)]
              )
            :➜leftmost ➜leftmost
            }))))
  (def-function-class ◨snw (tape n instance &optional ➜))
  (defun-typed ◨snw ((tape tape-empty) n instance &optional ➜)
    (declare (ignore n))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
         )
      ➜
      [➜empty]
      ))
  (defun-typed ◨snw ((tape tape-active) n instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜leftmost (λ()(error 'step-from-leftmost)))
        &allow-other-keys
        )
      ➜
      (let(
            (leftmost (leftmost tape))
            )
        (left-neighbor-n leftmost (- n)
          {
            :➜ok 
            (λ(the-left-neighbor)
              [➜ok (w<cell> the-left-neighbor instance)]
              )
            :➜leftmost ➜leftmost
            }))))

;;--------------------------------------------------------------------------------
;; topology manipulation
;;
  ;; prepends tape0 to tape1
  (def-function-class epa<tape> (tape1 tape0))
  (defun-typed epa<tape> ((tape1 tape) (tape0 tape-empty)) (do-nothing))

  ;; inserts the given cell as a new leftmost cell
  (def-function-class epa<cell> (tape cell))

  ;; makes and inserts a new leftmost cell initialized to the given instance
  (def-function-class epa<instance> (tape instance))

  ;; for a doubly linked list, these are the 'operate on tail' versions of the above
  (def-function-class ◨a<cell> (tape cell))
  (def-function-class ◨a<instance> (tape instance))

  ;; removes the leftmost cell and returns it
  ;; (➜ok #'echo) (➜rightmost (be ∅))
  (def-function-class epd<tape> (tape &optional ➜))
  (defun-typed epd<tape> ((tape tape-empty) &optional ➜)
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
  (def-function-class epd+<tape> (tape &optional ➜))
  (defun-typed epd+<tape> ((tape tape-empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜rightmost (λ()(error 'dealloc-on-rightmost)))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  ;; deletes rightmost
  ;; (➜ok #'echo) (➜leftmost (be ∅))
  (def-function-class ep-d<tape> (tape &optional ➜))
  (defun-typed ep-d<tape> ((tape tape-empty) &optional ➜)
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
    (d.<cell> (leftmost tape) ➜)
    )

  ;; returns right neighbor cell
  ;; (➜ok #'echo) (➜rightmost (λ()(error 'dealloc-on-rightmost)))
  ;; cell must be on the tape
  (def-function-class d+<tape> (tape cell &optional ➜))
  (defun-typed d+<tape> ((tape tape-empty) cell &optional ➜)
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
                
     

     
     
