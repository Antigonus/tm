#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Implementation of cell intended for use with a bidirectional list.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type definition
;;
  (def-type cell-bilist (cell-list left-neighbor cell)())

  (def-type bilist-leftmost-interior
    (
      cell-bilist 
      list-leftmost-interior
      )())
  (def-type bilist-rightmost-interior
    (
      cell-bilist
      list-rightmost-interior
      )())
  (def-type bilist-interior 
    (
      bilist-leftmost-interior
      bilist-rightmost-interior
      list-interior
      )())
  (def-type bilist-leftmost 
    (
      bilist-leftmost-interior
      list-leftmost
      )())
  (def-type bilist-rightmost
    (
      bilist-rightmost-interior 
      list-rightmost
      )())
  (def-type bilist-solitary  
    (
      bilist-rightmost 
      bilist-leftmost
      list-solitary
      )())
    
  (defun-typed to-cell      ((cell cell-bilist))(change-class cell 'cell-bilist))
  (defun-typed to-interior  ((cell cell-bilist))(change-class cell 'bilist-interior))
  (defun-typed to-leftmost  ((cell cell-bilist))(change-class cell 'bilist-leftmost))
  (defun-typed to-rightmost ((cell cell-bilist))(change-class cell 'bilist-rightmost))
  (defun-typed to-solitary  ((cell cell-bilist))(change-class cell 'bilist-solitary))

  (defun-typed init ((cell cell-bilist) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        left-neighbor
        &allow-other-keys
        )
      ➜
      (call-next-method cell instance 
        {
          :➜ok (λ(cell)
                 (cond
                   (left-neighbor (setf (left-neighbor cell) left-neighbor))
                   (t             (setf (left-neighbor cell) cell))
                   )
                 [➜ok cell]
                 )
          (o ➜)
          })
      ))

;;--------------------------------------------------------------------------------
;; cell functions
;;
  (defun-typed =<cell> ((cell-0 cell-bilist) (cell-1 cell-bilist) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      (call-next-method cell-0 cell-1 
        {
          :➜t
          (λ()
            (if
              (eq (left-neighbor cell-0) (left-neighbor cell-1))
              [➜t]
              [➜∅]
              ))
          :➜∅ ➜∅
          })))

  (defun-typed neighbor((cell cell-bilist) &optional ➜)
    (destructuring-bind
      (&key
        (➜bad-direction (λ()(error 'bad-direction)))
        (direction *right*)
        (distance 1)
        &allow-other-keys
        )
      ➜
      (cond
        ((= direction *right*) t)
        ((= direction *left*) (setf distance (- distance)))
        (t (return-from neighbor [➜bad-direction]))
        )
      (cond
        ((≥ distance 0)
          (neighbor-1 cell #'right-neighbor distance ➜)
          )
        (t ; distance must be negative
          (neighbor-1 cell #'left-neighbor distance ➜)
          )
        )))

;;--------------------------------------------------------------------------------
;; pimitive topology manipulation
;;

  ;; terminate the connections of a cell so that it can not affect gc
  ;;
    (defun-typed cap-left ((c cell-bilist))
      (setf (left-neighbor c) ∅)
      (to-leftmost c)
      )
    (defun-typed cap ((c cell-bilist)) 
      (setf (right-neighbor c) ∅)
      (setf (left-neighbor c) ∅)
      (to-cell c)
      )

  ;; c0 and c1 are two cells to be connected
  ;;
    (defun-typed connect ((c0 cell-bilist)(c1 cell-bilist))
      (setf (right-neighbor c0) c1)
      (setf (left-neighbor c1) c0)
      )

  ;; disconnect implemented in list.lisp works fine for bilists, because it is
  ;; based caps

  ;; c1 and c2 are neighbors.  re-routes connections around cell c1
  ;;
    (def-function-class -extract (c0 c1))
    (defun-typed -extract ((c1 bilist-leftmost) (c2 bilist-rightmost))
      (cap-left c2)
      (to-solitary c2)
      (cap c1)
      )
    (defun-typed -extract ((c1 bilist-interior) (c2 bilist-rightmost))
      (cap-left c2)
      (cap c1)
      )
    (defun-typed -extract ((c1 bilist-leftmost-interior) (c2 bilist-interior))
      (let(
            (c0 (left-neighbor c1))
            )
        (connect c0 c2)
        ;; c0 and c2 each maintains its status
        (cap c1)
        ))

;;--------------------------------------------------------------------------------
;; topology manipulation, interface implementations
;;
  ;; c2 and c0 are neighbors, inserts c1 between them
  ;;
    (def-function-class -a<cell> (c0 c1))
    (defun-typed -a<cell> ((c0 bilist-solitary) (c1 cell-bilist))
      (cap-left c1)
      (connect c1 c0)
      (to-rightmost c0)
      )
    (defun-typed -a<cell> ((c0 bilist-leftmost) (c1 cell-bilist))
      (cap-left c1)
      (connect c1 c0)
      (to-interior c0)
      )
    (defun-typed -a<cell> ((c0 bilist-rightmost-interior) (c1 cell-bilist))
      (let(
            (c2 (left-neighbor c0))
            )
        (connect c2 c1)
        (connect c1 c0)
        (to-interior c1)
        ;; c0 keeps its status
      ))

  ;; Deletes the left neighbor cell.
  ;; leftmost handled on the interface.
  ;; solitary, though this has the same behavior for all cells, had to be put 
  ;; here as otherwise CLOS would choose bilist-rightmost-interior as being more specific
  ;;
    (defun-typed -d<cell> ((cell bilist-solitary) &optional ➜)
      (destructuring-bind
        (&key
          (➜leftmost (λ()(error 'dealloc-on-leftmost)))
          &allow-other-keys
          )
        ➜
        [➜leftmost]
        ))
    (defun-typed -d<cell> ((c2 bilist-rightmost-interior) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          &allow-other-keys
          )
        ➜
        (let(
              (c1 (left-neighbor c2))
              )
          (-extract c1 c2)
          [➜ok c1]
          )))
   

