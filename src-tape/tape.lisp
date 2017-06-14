#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Architectural definition of a tape.

This tape machine interface does not take into account entanglements or threads.  These
things must be enforced externally.

Though these are tapes, not tape machines, function names found here appear to refer to
head operations. For example, 'e-s*' which reads as entanglement copy and step left to
leftmost.  These names are used only because they are descriptive of what the
corresponding function does to the tape.

Not all tapes implement all of the interface.  The tapes only implement the interface
portions that are primitive relative to the implementation.  As an example, the
singly linked list has no left going operations.

When the call interface of a tape function differes from its tape machine analog
we append <tape> or <cell> to its name.

We pushe end cases into dispatch.  Dispatch already makes decisions based
on operand type, and it won't mind having a few more types to work with.

|# 

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-type cell ()()) ; a union of cell types

  (def-type tape ()()) ; a union of tape types
  (def-type tape-active (tape)()) ; a union of tape types
  (def-type tape-empty (tape)()) ; a union of tape types
  (def-function-class to-active (tape))
  (def-function-class to-empty (tape))


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
  ;; can also be customized by parameters then there will have to be a more specific
  ;; version of empty init so as to catch those paramters.
  ;;
    (defun-typed init ((tape-1 tape) (tape-0 tape-empty) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          &allow-other-keys
          )
        ➜
        (to-empty tape-1)
        [➜ok tape-1]
        ))

   ;; It is possible to write a generic tape copy initializer for tapes that support toplogy
   ;; modification and do not take customization parameters by making use of #'epa.
   ;;
   ;; This would be its call interface:
   ;;   (defun-typed init ((tape-1 tape) (tape-0 tape-active) &optional ➜)

   

;;--------------------------------------------------------------------------------
;; accessing instances
;;
  (def-function-class e-s*r (tape &optional ➜))
  (defun-typed e-s*r ((tape tape-empty) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed e-s*r ((tape tape-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (r<cell> (leftmost tape))]
      ))

  ;; (➜ok #'echo) (➜rightmost (be ∅))
  (def-function-class e-s*sr (tape &optional ➜))
  (defun-typed e-s*sr ((tape tape-empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed e-s*sr ((tape tape-active) &optional ➜)
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

  (def-function-class e-s*w (tape instance &optional ➜))
  (defun-typed e-s*w ((tape tape-empty) instance &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed e-s*w ((tape tape-active) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (w<cell> (leftmost tape) instance)]
      ))


  ;; (➜ok #'echo) (➜rightmost (be ∅))
  (def-function-class e-s*sw (tape instance &optional ➜))
  (defun-typed e-s*sw ((tape tape-empty) instance &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
         )
      ➜
      [➜empty]
      ))
  (defun-typed e-s*sw ((tape tape-active) instance &optional ➜)
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
              [➜ok (w<cell> the-right-neighbor instance)]
              )
            :➜rightmost ➜rightmost
            }))))


  ;; for doubly linked lists we also have:

  (def-function-class es*r (tape &optional ➜))
  (defun-typed es*r ((tape tape-empty) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed es*r ((tape tape-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (r<cell> (rightmost tape))]
      ))

  (def-function-class es*-sr (tape &optional ➜))
  (defun-typed es*-sr ((tape tape-empty) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed es*-sr ((tape tape-active) &optional ➜)
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

  (def-function-class es*w (tape instance &optional ➜))
  (defun-typed es*w ((tape tape-empty) instance &optional ➜)
    (declare (ignore tape instance))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed es*w ((tape tape-active) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (w<cell> (rightmost tape) instance)]
      ))

  (def-function-class es*-sw (tape instance &optional ➜))
  (defun-typed es*-sw ((tape tape-empty) instance &optional ➜)
    (declare (ignore tape instance))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed es*-sw ((tape tape-active) instance &optional ➜)
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
;; topology queries
;;
  (def-function-class =<cell> (cell-0 cell-1))

  (def-function-class r<cell> (cell)) ; returns an instance
  (def-function-class w<cell> (cell instance))

  (def-function-class leftmost (tape &optional ➜)) ; returns a cell
  (defun-typed leftmost ((tape tape-empty) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))

  ;; (➜ok #'echo) (➜rightmost (be ∅))
  (def-function-class right-neighbor (cell &optional ➜))

  ;; for doubly linked lists we also have:

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
  (def-function-class epd<tape> (tape &optional ➜))
  (defun-typed epd<tape> ((tape tape-empty) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜rightmost (λ()(error 'dealloc-on-rightmost)))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  ;; for a doubly linked list, these are the 'operate on tail' versions of the above
  (def-function-class es*a<cell> (tape cell))
  (def-function-class es*a<instance> (tape instance))
  ;; (➜ok #'echo) (➜leftmost (be ∅))
  (def-function-class es*-sd<tape> (tape &optional ➜))
  (defun-typed es*-sd<tape> ((tape tape-empty) &optional ➜)
    (declare (ignore tape))
    (destructuring-bind
      (&key
        (➜leftmost (λ()(error 'dealloc-on-leftmost)))
        &allow-other-keys
        )
      ➜
      [➜leftmost]
      ))

  ;; given a cell removes its right neighbor and returns it
  ;; (➜ok #'echo) (➜rightmost (λ()(error 'dealloc-on-rightmost)))
  (def-function-class d<cell> (cell &optional ➜))

  ;; left neighbor version for doubly linked lists
  ;; (➜ok #'echo) (➜leftmost (be ∅))
  (def-function-class -d<cell> (cell &optional ➜))

  ;; If there is no rightneighbor, failes with ➜rightneighbor.
  ;; Swaps instances with the rightneighbor, then deletes the rightneighbor.
  ;; Returns the deleted rightneighbor after the instance swap.
  ;; Creates the appearence of deleting 'this cell' even for a singly linked list.
  ;; This is not needed for doubly linked lists, which can simply use 's-d'.
  ;;
  ;; (➜ok #'echo) (➜rightmost  (λ()(error 'dealloc-on-rightmost))).
  (def-function-class d.<cell> (cell &optional ➜))
  (defun-typed d.<cell> ((cell-0 cell) &optional ➜)
    (destructuring-bind
      (&key
        (➜rightmost (λ()(error 'dealloc-on-rightmost)))
        &allow-other-keys
        )
      ➜
      (let(
            (cell-0-instance (r<cell> cell-0))
            )
        (right-neighbor cell-0
          {
            :➜ok
            (λ(cell-1)
              (let(
                    (cell-1-instance (r<cell> cell-1))
                    )
                (w<cell> cell-0 cell-1-instance)
                (w<cell> cell-1 cell-0-instance)
                (d<cell> cell-0 ➜)
                ))
            :➜rightmost ➜rightmost
            })
        )))

  ;; appears to delete the leftmost cell, but doesn't, hence avoiding sharing issues
  ;; however external references to the right neighbor of leftmost become orphaned
  ;; (➜ok #'echo) (➜empty #'accessed-empty)
  (def-function-class e-s*d.<tape> (tape &optional ➜))
  (defun-typed e-s*d.<tape> ((tape tape-empty) &optional ➜)
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
;; length-tape
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
                
     

     
     
