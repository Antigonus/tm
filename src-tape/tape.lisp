#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Architectural definition of a tape.

This tape interface does not take into account entanglements or threads.  These
things must be enforced externally.

These are tapes, not tape machines, function names found here appear to refer to head
operations. For example, '◧sr' which reads as entanglement copy, step left to leftmost,
step right, and read.  These names are used only because they are descriptive of what the
corresponding function does, not because there is a tape head.  In this case, ◧sr, reads
the second instance on the tape.

Because these are tapes, and not tape machines, I avoided the temptation to implement
iteration here.  I have come back and added right-nieghbor-n and left-neighbor-n.  This is
because it is easy to add 'n' to an array index for the arrays. (it is in cell.lisp) and I
have a shallow copy because I need it for init.

Not all tapes implement all of the interface.  The tapes only implement the interface
portions that are primitive relative to the implementation.  As an example, the singly
linked list has no left going operations.

When the call interface of a tape function differes from its tape machine analog we append
<tape> or <cell> to its name.

We push end cases into dispatch.  Dispatch already makes decisions based on operand type,
and it won't mind having a few more types to work with.

|# 

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type
;;
  (def-type tape ()()) ; a union of tape types
  (def-type tape-abandoned (tape)()) 
  (def-type tape-active (tape)()) 
  (def-type tape-empty (tape)())

  (def-function-class to-abandoned (tape))
  (def-function-class to-empty     (tape))
  (def-function-class to-active    (tape))

  ;;----------------------------------------
  ;; generic init
  ;;
  ;; Generic init methods are difficult in general. This is because init's whole purpose
  ;; is to set up the custom type.
  ;;

  ;;   note that in Common Lisp (typep ∅ 'sequence) -> t
  ;;   I would argue this is a design flaw, because dispatch is all about choosing
  ;;   different functions for end cases.  Because (typep ∅ 'sequence) is true, we must
  ;;   explicitly test for null sequences in guard code.
  ;;
  ;;   Note, Mathematica will let us add additional tests into dispatch, so we can
  ;;   also have a special function for sequence length of zero:
  ;;   (defun-typed init ((tape tape) (seq?(= 0 (length seq)) sequence) &optional ➜)
  ;;    ..  but CLOS does not, so we must build the (= 0 length) test into guard code for
  ;;    every specialization (as opposed to specifying it in the argument list for every
  ;;    specialization -- LOL, seems sequence length zero should be another type also.)
  ;;
  ;; (defun-typed init ((tape tape) (init null) &optional ➜)

  ;; In general for intialization to an empty machine, for machines that can modify
  ;; topology, the function #'epa can transition them to tape-active.  If such machines
  ;; can also be customized by parameters then there will have to be a more specialized
  ;; version of empty init so as to catch those paramters.
  ;;
    (defun-typed init ((tape-1 tape) (tape-0 tape-empty) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          &allow-other-keys
          )
        ➜
        (to-empty tape-1) ;; this might not be sufficient to make a machine empty
        [➜ok tape-1]
        ))

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
  (defsynonym p-d<tape> ◨-sd<tape>)
  (def-function-class p-d<tape> (tape &optional ➜))
  (defun-typed p-d<tape> ((tape tape-empty) &optional ➜)
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
    (d.<cell> (lefmost tape) ➜)
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
  (def-function-class tape-length-is-one (tape &optional ➜))
  (defun-typed tape-length-is-one ((tape tape-empty) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))
  (defun-typed tape-length-is-one ((tape tape-active) &optional ➜)
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
  (defun-typed tape-length-is-two ((tape tape-empty) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))
  (defun-typed tape-length-is-two ((tape tape-active) &optional ➜)
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
                
     

     
     
