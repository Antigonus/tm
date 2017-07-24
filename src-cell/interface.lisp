#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Architectural definition of a cell.

We use the language of tape machines to name some of the cell functions. This reflects the
cells replationship to tapes, and in turn to tape machines.  It does not mean that tape
machine constructs, such a tape head, are involved in the implementation of the function.

A virtual cell is an emulation of a cell. We don't find such cells on a tape. Hence,
setting an attribute on a virtual cell will not be persistent. An example virtual
cell is one that holds a refrence to an array and an index.  We can use this cell on the
interface, but you won't find it in the array.  This stands in contrast with a list
cell, which comes directly from memory.

Cells are typically not first class citizens, rather each cell is part of one or more
regions.  The region provides bounding information.



|# 

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; type
;;
  ;; In some special operations we may need to know if a cell is being emulated
  ;; or if it is real.  'real is already a global symbol, so I use 'substrate' here.
  ;; Each cell implementation should carry one of these two attributes.
  ;;
    (def-type virtual ()())
    (def-type substrate ()())
    (def-type cell ()()) 

   ;; Cell's are typically not first class citizens, rather they always occur within
   ;; regions.  We have a separate file for operations unique to regions, but our
   ;; #'neighbor function needs to know bound locations, so it is defined here.
   ;;
     (def-type region ()
       (left-bound
         :initarg :left-bound
         :accessor left-bound
         )
       (right-bound
         :initarg :right-bound
         :accessor right-bound
         ))

;;--------------------------------------------------------------------------------
;; init


  ;; some cell types will have a different implementation
  (defun-typed clone ((cell cell) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let(
            (new-cell (mk (type-of cell) (r cell)))
            )
        [➜ok new-cell]
        )))

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
  ;; accepts option: n, and continutations: ➜ok ➜left-bound ➜right-bound
  ;; ➜ok has one parameter: the neighbor cell that was looked up
  ;; option n defaults to 1
  ;;
    (def-function-class neighbor (cell region &optional ➜))

    (defun apply-to-neighbor (cell region f &optional ➜)
      (neighbor cell region
        {
          :➜ok  (λ(neighbor-cell) [f neighbor-cell])
          (o ➜)
          }))

  (def-function-class r (cell &optional ➜))
  (defun-typed r ((cell cell) &optional ➜)
    (destructuring-bind
      (&key
        (n 0)
        region
        &allow-other-keys
        )
      ➜
      (cond
        ((= n 0) (r<cell> cell ➜))
        (t
          (apply-to-neighbor cell (λ(nc)(r<cell> nc ➜)) ➜)
          )
        )))

  (def-function-class w (cell instance &optional ➜))
  (defun-typed w ((cell cell) instance &optional ➜)
    (destructuring-bind
      (&key
        (n 0)
        region
        &allow-other-keys
        )
      ➜
      (cond
        ((= n 0) (r<cell> cell ➜))
        (t
          (apply-to-neighbor cell (λ(nc)(r<cell> nc ➜)) ➜)
          )
        )))



;;--------------------------------------------------------------------------------
;; topology manipulation
;;

  ;; makes cell-1 a right-neighbor of cell-0
  (def-function-class a<cell> (cell-0 cell-1 &optional ➜))

  ;; makes a new right neighbor for cell, and initializes it with instance.
  (def-function-class a (cell instance &optional ➜))
  (defun-typed a ((c0 cell) instance  &optional ➜)
    (let(
          (new-cell (make-instance (type-of c0) :contents instance))
          )
      (a<cell> c0 new-cell ➜)
      ))

  ;; given a cell removes its right neighbor and returns it
  ;; (➜ok #'echo) (➜right-bound (λ()(error 'dealloc-on-right-bound)))
  ;;
  ;; d<cell> for non-right-bound cells must be handled by implementations becasue
  ;; we don't know if the right neighbor is right-bound and do not know the relationship 
  ;; between right-bound and the list header.
  (def-function-class d<cell> (cell region &optional ➜))

  ;; The 'swap trick'
  ;;
  ;; If there is no right neighbor, fails with ➜right-bound.
  ;;
  ;; Swaps instances with the right neighbor, then deletes the right neighbor.
  ;;
  ;; Creates the appearence of deleting 'this cell' even for a singly linked list,
  ;; though we can't use it to delete right-bound.
  ;; This is not needed for doubly linked lists, which can simply use 's-d'.
  ;;
  ;; Interesting relationship with external references to the two cells involved.
  ;;
  ;; (➜ok #'echo) (➜right-bound  (λ()(error 'dealloc-on-right-bound))).
  ;;
    (def-function-class d.<cell> (cell region &optional ➜))
    (defun-typed d.<cell> ((c0 cell) (region region) &optional ➜)
      (destructuring-bind
        (&key
          (➜right-bound (λ()(error 'dealloc-on-right-bound)))
          &allow-other-keys
          )
        ➜
        (neighbor c0
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
            :➜right-bound ➜right-bound
            })))

  ;; Returns the right neighbor cell which is still connected to the rest of the tape.
  ;;
    (def-function-class d+<cell> (cell (region region) &optional ➜))

