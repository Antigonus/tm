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
  ;; one of these two attributes should be given to cell implementations
  ;;
    (def-type virtual ()())
    (def-type real ()())

  (def-type cell ()()) 

  ;; these are only used as typed function argument specifiers
  ;;
    (def-type leftmost-interior (cell)())
    (def-type rightmost-interior (cell)())

  ;; all cells appearing on a tape have exactly one of these subtypes
  ;;
    (def-type interior (leftmost-interior rightmost-interior)())
    (def-type leftmost (leftmost-interior)())
    (def-type rightmost (rightmost-interior)())
    (def-type solitary (leftmost rightmost)())

  (def-function-class to-cell (cell))
  (def-function-class to-interior (cell))
  (def-function-class to-leftmost (cell))
  (def-function-class to-rightmost (cell))
  (def-function-class to-solitary (cell))

;;--------------------------------------------------------------------------------
;; init
;; (➜ok #'echo) (➜bad (λ()(error 'bad-init-value))) (➜no-alloc #'alloc-fail)
;; 
  (def-function-class init (tape-instance init &optional ➜))

  (defun mk (tape-type init &optional ➜)
    (let(
          (tape-instance (make-instance tape-type))
          )
      (init tape-instance init ➜)
      ))

;;--------------------------------------------------------------------------------
;; cell functions
;;
  ;; same links and cargo
  (def-function-class =<cell> (cell-0 cell-1 &optional ➜))

  (def-function-class r (cell &optional ➜)) ; returns contents of cell
  (def-function-class w (cell instance &optional ➜)) ; writes contents of cell


;;--------------------------------------------------------------------------------
;; cell neighbor functions
;;
  ;; accepts :direction and :distance options, and the continutations, ➜ok ➜leftmost ➜rightmost
  ;; ➜ok has one parameter, the neighbor cell that was looked up
  ;;
    (def-function-class neighbor (cell &optional ➜))

  (defun apply-to-neighbor (cell f &optional ➜)
    (neighbor cell
      {
        :➜ok  (λ(neigbhor-cell) [f neighbor-cell])
        (o ➜)
        }))

  ;; It is conventional to have an indexed read and write for arrays.
  ;; accepts options :direction and :distance 
  ;; :direction defaults to zero, which is normally to the right
  ;; :distance defaults to 1
  ;;
    (def-function-class esr (cell &optional ➜))
    (defun-typed esnr ((cell cell) &optional ➜)
      (apply-to-neighbor cell (λ(nc)(r nc ➜)) ➜)
      )

    (def-function-class esw (cell n instance &optional ➜))
    (defun-typed esnw ((cell cell) n instance &optional ➜)
      (apply-to-neighbor cell (λ(nc)(w nc instance ➜)) ➜)
      )


;;--------------------------------------------------------------------------------
;; topology manipulation
;;

  ;; makes cell-1 a right-neighbor of cell-0
  (def-function-class a<cell> (cell-0 cell-1))
  ;; makes cell-1 a left-neighbor of cell-0
  (def-function-class -a<cell> (cell-0 cell-1))

  ;; makes a new right neighbor for cell, and initializes it with instance.
  (def-function-class a<instance> (cell instance))
  (defun-typed a<instance> ((c0 cell) instance)
    (let(
          (new-cell (make-instance (type-of c0) :contents instance))
          )
      (a<cell> c0 new-cell)
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
  ;; than d*. '+' means one or more applications. If applications is not possible, we
  ;; take the error path ➜rightmost
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

