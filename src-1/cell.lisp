#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Architectural definition of a cell.

Cells must be creatable with mk, and must be clonable.

Cells are typically not first class citizens, rather each cell is part of one or more
'worlds'.  The world context provides references to bounding cells and other information.
Cells are strung together to make 'tapes'.

Cells may carry certain attributes such as being virtual, subspace nodes, or world
nodes.

A virtual cell is an emulation of a cell. Though they act like they are on the tape, and
work with the interface, you won't actually find such cells on a tape.  An example virtual
cell is one for an array.  Such a cell will probably hold an array index and a reference
to the array base.  We can use this cell on the interface, but you won't find it in the
array itself. There are some simularities to an iterator in other languages. This stands
in contrast with a list cell, which comes directly from memory.

Subspace cells have an additional field for holding another tape.  The instances of
the cells of this other tape are themselves tapes. Subspace cells are used for
creating tree structures.

A multiple world cell holds a decision point in multiple world view of a tape. A multiple
world view cell potentially has multiple cells, where each cell is looked up by the
world id.  A given cell that is not marked as being multiple world belongs to the same
world as the neighbor cell that was used to traverse to the given cell. One cell
can belong to multiple worlds.  An example implementation of a multiple world cell
is a hash on world id of cells. 

Each world is identified with a 'context' instance.

Many operations take an option understood as a number of neighbor to neighbor steps to be
taken.  This is the same thing as an address difference; hence we call it 'Δ'.  For singly
linked lists, by contract with the programmer, Δ is a Natural number, i.e. a count of zero
or more steps to be taken. For doubly linked tapes Δ may carry a sign.  In other cases Δ
may be a vector that is used as an address difference, or even a tape machine that
generates such successive differences.

Subspace and multiple world cells are further developed in src-cell-1 where more of the tm
interface is available. Oh I better start calling them 'multiple-universe' rather than
multiple world, because 'w' is already used for write.

|# 

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; type
;;
  ;; In some special operations we may need to know if a cell is being emulated,
  ;; i.e. virtual, or if it is real.  'real is already a global symbol, so I use
  ;; 'substrate' here.  Each cell implementation should carry one of those two attributes.
  ;; Other attributes including being a subspace cell and a world-cell
  ;;
    (def-type cell-virtual ()())
    (def-type cell-substrate ()())
    (def-type cell ()()) ; contents direct, link direct
    (def-type cell-simplex (cell)()) ; contents and links are direct
    (def-type cell-multiplexed (cell)()) ; contents is a table, link might be simplex or a table

;;--------------------------------------------------------------------------------
;; init
;;
  ;; we don't clone cell-alternative type cells, so no context is required
  (defun-typed clone ((cell cell) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (mk (type-of cell) (r cell) ➜)
      ))

;;--------------------------------------------------------------------------------
;; cell functions
;;
  ;; same links and contents
  (def-function-class =<cell> (cell-0 cell-1 &optional ➜))
  (def-function-class r<cell> (cell &optional ➜)) ; returns contents of cell
  (def-function-class w<cell> (cell instance &optional ➜)) ; writes contents of cell

;;--------------------------------------------------------------------------------
;; cell neighbor functions
;;
  ;; accepts option: n, and continutations: ➜ok ➜bound 
  ;; ➜ok has one parameter: the neighbor cell that was looked up
  ;; option n defaults to 1
  ;;
    (def-function-class neighbor (context cell &optional ➜))
    (defun-typed neighbor ((context context) (cell cell-multiplexed) &optional ➜)
      (neighbor context (wr<cell> context cell) ➜)
      )
    (defun apply-to-neighbor (context cell f &optional ➜)
      (neighbor context cell
        {
          :➜ok  (λ(neighbor-cell) [f neighbor-cell])
          (o ➜)
          }))


;;--------------------------------------------------------------------------------
;; topology manipulation
;;

  ;; makes cell-1 a right-neighbor of cell-0
  (def-function-class a<cell> (context cell-0 cell-1 &optional ➜))

  ;; makes a new right neighbor for cell, and initializes it with instance.
  (def-function-class a (context cell instance &optional ➜))
  (defun-typed a (context (c0 cell) instance  &optional ➜)
    (let(
          (new-cell (mk (type-of c0) instance))
          )
      (a<cell> c0 new-cell ➜)
      ))

  ;; given a cell removes its right neighbor and returns it
  ;; (➜ok #'echo) (➜bound-right (λ()(error 'dealloc-on-bound-right)))
  ;;
  ;; d<cell> for non-bound-right cells must be handled by implementations becasue
  ;; we don't know if the right neighbor is bound-right and do not know the relationship 
  ;; between bound-right and the list header.
  (def-function-class d<cell> (context cell context &optional ➜))

  ;; The 'swap trick'
  ;;
  ;; If there is no right neighbor, fails with ➜bound-right.
  ;;
  ;; Swaps instances with the right neighbor, then deletes the right neighbor.
  ;;
  ;; Creates the appearence of deleting 'this cell' even for a singly linked list,
  ;; though we can't use it to delete bound-right.
  ;; This is not needed for doubly linked lists, which can simply use 's-d'.
  ;;
  ;; Interesting relationship with external references to the two cells involved.
  ;;
  ;; (➜ok #'echo) (➜bound-right  (λ()(error 'dealloc-on-bound-right))).
  ;;
    (def-function-class d.<cell> (context cell &optional ➜))
    (defun-typed d.<cell> (context (c0 cell) &optional ➜)
      (destructuring-bind
        (&key
          (➜bound-right (λ()(error 'dealloc-on-bound-right)))
          &allow-other-keys
          )
        ➜
        (neighbor context c0
          {
            :➜ok
            (λ(c1)
              (let(
                    (c0-instance (r c0))
                    (c1-instance (r c1))
                    )
                (w c0 c1-instance)
                (w c1 c0-instance)
                (d<cell> c0 ➜)
                ))
            :➜bound-right ➜bound-right
            })))

  ;; Returns the right neighbor cell which is still connected to the rest of the tape.
  ;;
    (def-function-class d+<cell> (cell (region region) &optional ➜))

;;--------------------------------------------------------------------------------
;; multiple world support
;;
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
