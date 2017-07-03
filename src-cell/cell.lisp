#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Architectural definition of a cell.

A cell is not a first class citizen.  Rather we consider that a cell is orphaned if it is
not part of a tape.  Orphaned cells are locally placed on new tapes or are quickly
abandoned.  Hence, we name a cell after the type of tape it is used on.

A cell's subtype speaks to its position on a tape. A leftmost cell is the left end of a
tape.  The rightmost cell is the right end of a tape. If a tape is a solitary set of
cells, then the type of that one cell is solitary.  A solitary cell is both leftmost and
rightmost.

When the topology of a tape is modified, a cell's sub-type might change. For example, if
the rightmost cell of a tape is deleted, then the left neighbor of that former rightmost
cell becomes the new rightmost cell. Inversely, if a new cell is appended to the rightmost
cell of the tape, the new cell becomes rightmost, and the former rightmost cell reverts to
having no subtype, i.e. for cell c, (to-cell c).

We use the language of tape machines to name some of the cell functions. This reflects the
cells replationship to tapes, and in turn to tape machines.  It does not mean that tape
machine constructs, such a tape head, are involved in the implementation of the function.

|# 

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type
;;
  (def-type cell ()()) 

  (def-type leftmost-interior (cell)())
  (def-type rightmost-interior (cell)())

  (def-type interior (leftmost-interior rightmost-interior)())
  (def-type leftmost (leftmost-interior)())
  (def-type rightmost (rightmost-interior)())
  (def-type solitary (leftmost rightmost)())

  (def-function-class to-cell (cell))
  (def-function-class to-interior (cell))
  (def-function-class to-leftmost (cell))
  (def-function-class to-rightmost (cell))
  (def-function-class to-solitary (cell))

  (defmacro place-right (c0 new-right-neighbor)
    `(progn
       (setf (right-neighbor ,c0) ,new-right-neighbor)
       (setf (left-neighbor ,new-right-neighbor) ,c0)
       ))
  ;; c0 c1 are either neighbors, or the same cell
  ;; Because we use self-pointers instead of nil to terminate ends, insert
  ;; will work even against a single cell.
  (defmacro place-inbetween (c0 c1 new-cell)
    `(progn
       (place-right ,c0 ,new-cell)
       (place-right ,new-cell ,c1)
       ))

  (defmacro cap-left (c0)
    `(setf (left-neighbor ,c0) ,c0)
    )

  (defmacro cap-right (c0)
    `(setf (right-neighbor ,c0) ,c0)
    )

  (defmacro cap-off (c0)
    `(progn
       (cap-left ,c0)
       (cap-right ,c0)
       ))

  ;; c0 c1 c2 are neighbors in sequence
  ;; connects c0 to c2, caps off c1
  (defmacro extract (c0 c1 c2)
    `(progn
       (place-right ,c0 ,c2)
       (cap-off ,c1)
       ))

  ;; disconnects c0 from c1
  (defmacro disconnect (c0 c1)
    `(progn
       (cap-right ,c0)
       (cap-left ,c1)
       ))

  ;; cell is unitialized, having been freshly created using #'make-instance.
  (defun-typed init ((cell cell) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜fail (λ()(error 'bad-init-value)))
        type left-neighbor right-neighbor
        )
      ➜
      (w<cell> cell instance)
      ;; When these two conds are put together, SBCL erroneously deletes the (➜ left-neighbor) clause
      ;;
        (cond
          (right-neighbor     (place-right cell right-neighbor))
          ((¬ right-neighbor) (cap-right cell))
          )
        (cond
          (left-neighbor      (place-right left-neighbor cell))
          ((¬ left-neighbor)  (cap-left cell))
          )
      (if type
        (case type
          (interior  (to-interior  cell))
          (leftmost  (to-leftmost  cell))
          (rightmost (to-rightmost cell))
          (solitary  (to-solitary  cell))
          (otherwise (return-from init [➜fail]))
          )
        (to-interior cell)
        )
      [➜ok cell]
      ))


;;--------------------------------------------------------------------------------
;; queries
;;
  (def-function-class =<cell> (cell-0 cell-1 &optional ➜))
  (defun-typed =<cell> ((cell-0 cell) (cell-1 cell) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      (if
        (eq cell-0 cell-1)
        [➜t]
        [➜∅]
        )))

  (def-function-class r<cell> (cell)) ; returns contents of cell
  (def-function-class w<cell> (cell instance)) ; writes contents of cell

  ;; for bilist these are slots
  (def-function-class right-neighbor (cell))
  (def-function-class left-neighbor (cell))

  ;; then nth neighbor to the right
  ;; For arrays, this just increments the array index, which is why this is here
  ;; instead of being part of the tape machine.
  ;;
    (def-function-class right-neighbor-n (cell n &optional ➜))
    (defun-typed right-neighbor-n ((cell cell) n &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          (➜rightmost (λ(cell n)(declare (ignore cell n))(error 'step-from-rightmost)))
          &allow-other-keys
          )
        ➜
      (cond
        ((< n 0) (left-neighbor-n cell (- n) ➜))
        (t
          (loop 
            (cond
              ((= n 0) (return [➜ok cell]))
              ((typep cell 'rightmost)(return [➜rightmost cell n]))
              (t
                (setf n (1- n))
                (setf cell (right-neighbor cell))
                )))))))

    (def-function-class left-neighbor-n (cell n &optional ➜))
    (defun-typed left-neighbor-n (cell n &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          (➜leftmost (λ(cell n)(declare (ignore cell n))(error 'step-from-leftmost)))
          &allow-other-keys
          )
        ➜
      (cond
        ((< n 0) (right-neighbor-n cell (- n) ➜))
        (t
          (loop 
            (cond
              ((= n 0) (return [➜ok cell]))
              ((typep cell 'leftmost)(return [➜leftmost cell n]))
              (t
                (setf n (1- n))
                (setf cell (left-neighbor cell))
                )))))))


;;--------------------------------------------------------------------------------
;; advanced queries
;;

  ;; returns an instance
  (def-function-class esr<cell> (cell &optional ➜))
  (defun-typed esr<cell> ((cell rightmost) &optional ➜)
    (destructuring-bind
      (&key
        (➜rightmost (λ()(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))
  (defun-typed esr<cell> ((cell cell) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (r<cell> (right-neighbor cell))]
      ))
  (def-function-class esnr<cell> (cell n &optional ➜))
  (defun-typed esnr<cell> ((cell cell) n &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ(cell n)(declare (ignore cell n))(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (right-neighbor-n cell n
        {
          :➜ok (λ(rn)[➜ok (r<cell> rn)])
          :➜rightmost ➜rightmost
          })))


  (def-function-class esw<cell> (cell instance &optional ➜))
  (defun-typed esw<cell> ((cell rightmost) instance &optional ➜)
    (declare (ignore instance))
    (destructuring-bind
      (&key
        (➜rightmost (λ()(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))
  (defun-typed esw<cell> ((cell cell) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (w<cell> (right-neighbor cell) instance)]
      ))
  (def-function-class esnw<cell> (cell n instance &optional ➜))
  (defun-typed esnw<cell> ((cell cell) n instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ(cell n)(declare (ignore cell n))(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (right-neighbor-n cell n
        {
          :➜ok (λ(rn)[➜ok (w<cell> rn instance)])
          :➜rightmost ➜rightmost
          })))


;;--------------------------------------------------------------------------------
;; topology manipulation
;;
  ;; makes cell-1 a right-neighbor of cell-0
  (def-function-class a<cell> (cell-0 cell-1))
  ;;The solitary and rightmost cases must be handled by specialized implementation
  ;;functions.  We can't do it here because we don't know the relationship between
  ;;rightmost and the tape header for the implementation.
  (defun-typed a<cell> ((c0 leftmost-interior) (new-cell cell))
    (let(
          (c1 (right-neighbor c0)) ; c0 is not rightmost, but c1 might be
          )
      (place-inbetween c0 c1 new-cell)
      (to-interior new-cell)
      (values)
      ))

  ;; makes a new right neighbor for cell, and initializes it with instance.
  (def-function-class a<instance> (cell instance))
  (defun-typed a<instance> ((c0 cell) instance)
    (let(
          (new-cell (make-instance (type-of c0) :contents instance))
          )
      (a<cell> c0 new-cell)
      ))

  ;; makes cell-1 a left-neighbor of cell-0
  (def-function-class -a<cell> (cell-0 cell-1))
  ;;The solitary and leftmost cases must be handled by specialized implementation
  ;;functions.  We can't do it here because we don't know the relationship between
  ;;rightmost and the tape header for the implementation.
  (defun-typed -a<cell> ((c1 rightmost-interior) (new-cell cell))
    (let(
          (c0 (left-neighbor c1)) ; c1 is not leftmost, but c0 might be
          )
      (place-inbetween c0 c1 new-cell)
      (to-interior new-cell)
      (values)
      ))

  ;; makes a new left neighbor for cell, and initializes it with instance.
  (def-function-class -a<instance> (cell instance))
  (defun-typed -a<instance> ((c0 cell) instance)
    (let(
          (new-cell (make-instance (type-of c0) :contents instance))
          )
      (-a<cell> c0 new-cell)
      ))

  ;; given a cell removes its right neighbor and returns it
  ;; (➜ok #'echo) (➜rightmost (λ()(error 'dealloc-on-rightmost)))
  ;;
  ;; d<cell> for non-rightmost cells must be handled by implementations becasue
  ;; we don't know if the right neighbor is rightmost and do not know the relationship 
  ;; between rightmost and the list header.
  ;;
  ;; 'solitary' also goes here, because it is a specialization of rightmost
  ;;
    (def-function-class d<cell> (cell &optional ➜))
    (defun-typed d<cell> ((cell rightmost) &optional ➜)
      (destructuring-bind
        (&key
          (➜rightmost (λ()(error 'dealloc-on-rightmost)))
          &allow-other-keys
          )
        ➜
        [➜rightmost]
        ))

  ;; left neighbor version for doubly linked lists
  ;; (➜ok #'echo) (➜leftmost (be ∅))
  ;;
  ;; -d<cell> for non-rightmost cells must be handled by implementations becasue
  ;; we don't know if the right neighbor is rightmost and do not know the relationship 
  ;; between rightmost and the list header.
  ;;
  ;; 'solitary' also goes here, because it is a specialization of leftmost
  ;;
    (def-function-class -d<cell> (cell &optional ➜))
    (defun-typed -d<cell> ((cell leftmost) &optional ➜)
      (destructuring-bind
        (&key
          (➜leftmost (λ()(error 'dealloc-on-leftmost)))
          &allow-other-keys
          )
        ➜
        [➜leftmost]
        ))

  ;; The 'swap trick'
  ;;
  ;; If there is no right neighbor, fails with ➜rightneighbor.
  ;; Swaps instances with the right neighbor, then deletes the right neighbor.
  ;; Returns the deleted right neighbor after the instance swap.
  ;; Creates the appearence of deleting 'this cell' even for a singly linked list,
  ;; though we can't use it to delete rightmost.
  ;; This is not needed for doubly linked lists, which can simply use 's-d'.
  ;; Wreaks havoc when external pointer are pointing at the cells.
  ;;
  ;; (➜ok #'echo) (➜rightmost  (λ()(error 'dealloc-on-rightmost))).
  ;;
    (def-function-class d.<cell> (cell &optional ➜))
    (defun-typed d.<cell> ((cell-0 rightmost) &optional ➜)
      (destructuring-bind
        (&key
          (➜rightmost (λ()(error 'dealloc-on-rightmost)))
          &allow-other-keys
          )
        ➜
        [➜rightmost]
        ))
    (defun-typed d.<cell> ((cell-0 leftmost-interior) &optional ➜)
      (let*(
             (cell-0-instance (r<cell> cell-0))
             (cell-1 (right-neighbor cell-0))
             (cell-1-instance (r<cell> cell-1))
            )
        (w<cell> cell-0 cell-1-instance)
        (w<cell> cell-1 cell-0-instance)
        (d<cell> cell-0 ➜)
        ))

  ;; Returns the right neighbor cell which is still connected to the rest of the tape. A
  ;; tape implementation (not a cell implementation) might need its own version of this to
  ;; fix the righmost cell's relationships with the tape header. Kleene '*' can mean zero
  ;; applications, but in the ➜ok case we must return something, so this becomes d+ rather
  ;; than d*. '+' means one or more applications. If there can be no applications, we have
  ;; an error return, ➜rightmost
  ;;
    (def-function-class d+<cell> (cell &optional ➜))
    (defun-typed d+<cell> ((cell rightmost) &optional ➜)
      (destructuring-bind
        (&key
          (➜rightmost (λ()(error 'dealloc-on-rightmost)))
          &allow-other-keys
          )
        ➜
        [➜rightmost]
        ))
    (defun-typed d+<cell> ((cell leftmost-interior) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          &allow-other-keys
          )
        ➜
        (let(
              (rn (right-neighbor cell))
              )
          (disconnect cell rn)
          [➜ok rn]
          )))

