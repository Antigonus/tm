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
  (def-type bilink (link)
    (
      (left-neighbor
        :initarg :left-neighbor
        :accessor left-neighbor
        )
      ))
  (defparameter *left*  1)

  (def-type cell-bilist (cell-list bilink cell)())

  (def-type bilist-bound-left-interior
    (
      cell-bilist 
      list-bound-left-interior
      )())
  (def-type bilist-bound-right-interior
    (
      cell-bilist
      list-bound-right-interior
      )())
  (def-type bilist-interior 
    (
      bilist-bound-left-interior
      bilist-bound-right-interior
      list-interior
      )())
  (def-type bilist-bound-left 
    (
      bilist-bound-left-interior
      list-bound-left
      )())
  (def-type bilist-bound-right
    (
      bilist-bound-right-interior 
      list-bound-right
      )())
  (def-type bilist-solitary  
    (
      bilist-bound-right 
      bilist-bound-left
      list-solitary
      )())
    
  (defun-typed to-cell      ((cell cell-bilist))(change-class cell 'cell-bilist))
  (defun-typed to-interior  ((cell cell-bilist))(change-class cell 'bilist-interior))
  (defun-typed to-bound-left  ((cell cell-bilist))(change-class cell 'bilist-bound-left))
  (defun-typed to-bound-right ((cell cell-bilist))(change-class cell 'bilist-bound-right))
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
        (d *right*)
        (n 0)
        &allow-other-keys
        )
      ➜
      (cond
        ((= d *right*) t)
        ((= d *left*) (setf n (- n)))
        (t (return-from neighbor [➜bad-direction]))
        )
      (cond
        ((≥ n 0)
          (neighbor-1 cell #'right-neighbor n ➜)
          )
        (t ; distance must be negative
          (neighbor-1 cell #'left-neighbor n ➜)
          )
        )))

;;--------------------------------------------------------------------------------
;; pimitive topologypp manipulation
;;

  ;; terminate the connections of a cell so that it can not affect gc
  ;;
    (defun-typed cap-left ((c cell-bilist))
      (setf (left-neighbor c) ∅)
      (to-bound-left c)
      )
    (defun-typed cap ((c cell-bilist)) 
      (setf (right-neighbor c) ∅)
      (setf (left-neighbor c) ∅)
      (to-cell c)
      )

  ;; c0 and c1 are two cells to be connected
  ;;
    (defun-typed connect ((c0 bilink)(c1 bilink))
      (setf (right-neighbor c0) c1)
      (setf (left-neighbor c1) c0)
      )

  ;; disconnect implemented in list.lisp works fine for bilists, because it is
  ;; based caps

  ;; c1 and c2 are neighbors.  re-routes connections around cell c1
  ;;
    (def-function-class -extract (c0 c1))
    (defun-typed -extract ((c1 bilist-bound-left) (c2 bilist-bound-right))
      (cap-left c2)
      (to-solitary c2)
      (cap c1)
      )
    (defun-typed -extract ((c1 bilist-interior) (c2 bilist-bound-right))
      (cap-left c2)
      (cap c1)
      )
    (defun-typed -extract ((c1 bilist-bound-left-interior) (c2 bilist-interior))
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
  (defun-typed a<cell> ((c0 cell-bilist) (c1 cell-bilist) &optional ➜)
    (destructuring-bind
      (&key
        (➜bad-direction (λ()(error 'bad-direction)))
        (d *right*)
        &allow-other-keys
        )
      ➜
      (cond
        ((= d *right*) (call-next-method))
        ((= d *left*) (-a<cell> c0 c1))
        (t
          [➜bad-direction]
          ))))

    (def-function-class -a<cell> (c0 c1))
    (defun-typed -a<cell> ((c0 bilist-solitary) (c1 cell-bilist))
      (cap-left c1)
      (connect c1 c0)
      (to-bound-right c0)
      )
    (defun-typed -a<cell> ((c0 bilist-bound-left) (c1 cell-bilist))
      (cap-left c1)
      (connect c1 c0)
      (to-interior c0)
      )
    (defun-typed -a<cell> ((c0 bilist-bound-right-interior) (c1 cell-bilist))
      (let(
            (c2 (left-neighbor c0))
            )
        (connect c2 c1)
        (connect c1 c0)
        (to-interior c1)
        ;; c0 keeps its status
      ))

  (defun-typed d<cell> ((c0 cell-bilist) &optional ➜)
    (destructuring-bind
      (&key
        (➜bad-direction (λ()(error 'bad-direction)))
        (d *right*)
        &allow-other-keys
        )
      ➜
      (cond
        ((= d *right*) (call-next-method))
        ((= d *left*) (-d<cell> c0 ➜))
        (t
          [➜bad-direction]
          ))))


  ;; Deletes the left neighbor cell.
  ;; bound-left handled on the interface.
  ;; solitary, though this has the same behavior for all cells, had to be put 
  ;; here as otherwise CLOS would choose bilist-bound-right-interior as being more specific
  ;;
    (def-function-class -d<cell> (cell &optional ➜))
    (defun-typed -d<cell> ((cell bilist-solitary) &optional ➜)
      (destructuring-bind
        (&key
          (➜bound-left (λ()(error 'dealloc-on-bound-left)))
          &allow-other-keys
          )
        ➜
        [➜bound-left]
        ))
    (defun-typed -d<cell> ((c2 bilist-bound-right-interior) &optional ➜)
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
   

