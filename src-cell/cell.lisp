#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Architectural definition of a cell.

|# 

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type
;;
  (def-type cell ()()) ; a union of cell types
  (def-type cell-leftmost (cell)())
  (def-type cell-rightmost (cell)())
  (def-type cell-singleton (cell-leftmost cell-rightmost)())

  (def-function-class to-cell (cell))
  (def-function-class to-leftmost (cell))
  (def-function-class to-rightmost (cell))
  (def-function-class to-singleton (cell))


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

  ;; then nth neighbor to the right
  ;; For arrays, this just increments the array index, which is why this is here
  ;; instead of being part of the tape machine.
  ;;
    (defun right-neighbor-n (cell n &optional ➜)
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
              ((typep cell 'cell-rightmost)(return [➜rightmost cell n]))
              (t
                (setf n (1- n))
                (setf cell (right-neighbor cell))
                )))))))

    (defun left-neighbor-n (cell n &optional ➜)
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
              ((typep cell 'cell-leftmost)(return [➜leftmost cell n]))
              (t
                (setf n (1- n))
                (setf cell (left-neighbor cell))
                )))))))


;;--------------------------------------------------------------------------------
;; advanced queries
;;

  ;; returns an instance
  (def-function-class esr<cell> (cell &optional ➜))
  (defun-typed esr<cell> ((cell cell-rightmost) &optional ➜)
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
  (defun-typed esw<cell> ((cell cell-rightmost) instance &optional ➜)
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
          :➜ok (λ(rn)[➜ok (w<cell> rn) instance])
          :➜rightmost ➜rightmost
          })))


;;--------------------------------------------------------------------------------
;; topology manipulation
;;
  ;; makes cell-1 a right-neighbor of cell-0
  (def-function-class a<cell> (cell-0 cell-1))

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

  ;; makes a new left neighbor for cell, and initializes it with instance.
  (def-function-class -a<instance> (cell instance))

  ;; given a cell removes its right neighbor and returns it
  ;; (➜ok #'echo) (➜rightmost (λ()(error 'dealloc-on-rightmost)))
  (def-function-class d<cell> (cell &optional ➜))
  (defun-typed d<cell> ((cell cell-rightmost) &optional ➜)
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
  (def-function-class -d<cell> (cell &optional ➜))

  ;; If there is no right neighbor, fails with ➜rightneighbor.
  ;; Swaps instances with the right neighbor, then deletes the right neighbor.
  ;; Returns the deleted right neighbor after the instance swap.
  ;; Creates the appearence of deleting 'this cell' even for a singly linked list,
  ;; though we can't use it to delete rightmost.
  ;; This is not needed for doubly linked lists, which can simply use 's-d'.
  ;; Wreaks havoc when external pointer are pointing at the cells.
  ;;
  ;; (➜ok #'echo) (➜rightmost  (λ()(error 'dealloc-on-rightmost))).
  (def-function-class d.<cell> (cell &optional ➜))
  (defun-typed d.<cell> ((cell-0 cell-rightmost) &optional ➜)
    (destructuring-bind
      (&key
        (➜rightmost (λ()(error 'dealloc-on-rightmost)))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))
  (defun-typed d.<cell> ((cell-0 cell) &optional ➜)
    (let*(
           (cell-0-instance (r<cell> cell-0))
           (cell-1 (right-neighbor cell-0))
           (cell-1-instance (r<cell> cell-1))
          )
      (w<cell> cell-0 cell-1-instance)
      (w<cell> cell-1 cell-0-instance)
      (d<cell> cell-0 ➜)
      ))

  ;; Returns right neighbor cell, which is still connected to the rest of the tape. Tape
  ;; might need its own version of this to fix the righmost cell's relationships with the
  ;; tape header.
  ;; (➜ok #'echo) (➜rightmost (λ()(error 'dealloc-on-rightmost)))
  (def-function-class d*<cell> (cell &optional ➜))
  (defun-typed d*<cell> ((cell cell-rightmost) &optional ➜)
    (destructuring-bind
      (&key
        (➜rightmost (λ()(error 'dealloc-on-rightmost)))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

