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
  ;; Our tape header looks like a cell in that when the tape is not empty it has a right
  ;; neigibhor and leftneighbor.  The right neigbor is the leftmost cell of the tape, and
  ;; the leftneighbor is the rightmost cell of the tape. But it differs from a cell in that
  ;; it can not be read or written.
  ;;
  ;; I would prefer this to be a simple array of links, and to abstract a direction
  ;; concept as an index rather than to have 'left' and 'right'.
  ;; i.e.  (neighbor cell 0) (neighbor cell 1) etc.
  ;;
    (def-type bilink ()
      (
        (left-neighbor-slot
          :initarg :left-neighbor-slot
          :accessor left-neighbor-slot
          )
        (right-neighbor-slot
          :initarg :right-neighbor-slot
          :accessor right-neighbor-slot
          )
        ))

  (def-type cell-bilist (bilink cell)
    (
      (contents :initarg :contents :accessor contents)
      ))

  (def-type bilist-leftmost-interior (cell-bilist leftmost-interior)())
  (def-type bilist-rightmost-interior (cell-bilist rightmost-interior)())

  (def-type bilist-interior  (bilist-leftmost-interior bilist-rightmost-interior interior)())
  (def-type bilist-leftmost  (bilist-leftmost-interior leftmost)())
  (def-type bilist-rightmost (bilist-rightmost-interior rightmost)())
  (def-type bilist-solitary  (bilist-leftmost bilist-rightmost solitary)())

  (defun-typed to-cell      ((cell cell-bilist))(change-class cell 'cell-bilist))
  (defun-typed to-interior  ((cell cell-bilist))(change-class cell 'bilist-interior))
  (defun-typed to-leftmost  ((cell cell-bilist))(change-class cell 'bilist-leftmost))
  (defun-typed to-rightmost ((cell cell-bilist))(change-class cell 'bilist-rightmost))
  (defun-typed to-solitary  ((cell cell-bilist))(change-class cell 'bilist-solitary))

  (defmacro connect (c0 c1)
    `(progn
       (setf (right-neighbor-slot ,c0) ,c1)
       (setf (left-neighbor-slot ,c1) ,c0)
       ))
  ;; c0 c1 are either neighbors, or the same cell
  ;; Because we use self-pointers instead of nil to terminate ends, insert
  ;; will work even against a single cell.
  (defmacro insert-between (c0 c1 new-cell)
    `(progn
       (connect ,c0 ,new-cell)
       (connect ,new-cell ,c1)
       ))

  (defmacro cap-left (c0)
    `(setf (left-neighbor-slot ,c0) ,c0)
    )

  (defmacro cap-right (c0)
    `(setf (right-neighbor-slot ,c0) ,c0)
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
       (connect ,c0 ,c2)
       (cap-off ,c1)
       ))

  ;; disconnects c0 from c1
  (defmacro disconnect (c0 c1)
    `(progn
       (cap-right ,c0)
       (cap-left ,c1)
       ))

  (defun-typed init ((cell cell-bilist) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜fail (λ()(error 'bad-init-value)))
        status left-neighbor right-neighbor
        )
      ➜
      (w<cell> cell instance)

      ;; When these two conds are put together, SBCL currently, seemingly erroneously,
      ;; deletes the (➜ left-neighbor) clause
      ;;
        (cond
          (right-neighbor     (connect cell right-neighbor))
          ((¬ right-neighbor) (cap-right cell))
          )
        (cond
          (left-neighbor      (connect left-neighbor cell))
          ((¬ left-neighbor)  (cap-left cell))
          )
      (if status
        (case status
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
  (defun-typed r<cell> ((cell cell-bilist)) (contents cell))
  (defun-typed w<cell> ((cell cell-bilist) instance) (setf (contents cell) instance))

  (defun-typed right-neighbor ((cell bilist-leftmost-interior) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (right-neighbor-slot cell)]
      ))

  (defun-typed left-neighbor ((cell bilist-rightmost-interior) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (left-neighbor-slot cell)]
      ))


;;--------------------------------------------------------------------------------
;; topology manipulation
;;

  (defun-typed a<cell> ((c0 bilist-solitary) (new-cell cell-bilist))
    (let(
          (c1 (right-neighbor-slot c0)) ; this will be the list header, a bilink type
          )
      (insert-between c0 c1 new-cell)
      (to-leftmost c0)
      (to-rightmost new-cell)
      (values)
      ))
  (defun-typed a<cell> ((c0 bilist-rightmost) (new-cell cell-bilist))
    (let(
          (c1 (right-neighbor-slot c0)) ; this will be the list header, a bilink type
          )
      (insert-between c0 c1 new-cell)
      (to-interior c0)
      (to-rightmost new-cell)
      (values)
      ))
  (defun-typed a<cell> ((c0 leftmost-interior) (new-cell cell))
    (let(
          (c1 (right-neighbor c0)) ; c0 is not rightmost, but c1 might be
          )
      (insert-between c0 c1 new-cell)
      (to-interior new-cell)
      (values)
      ))


  (defun-typed -a<cell> ((c1 bilist-solitary) (new-cell cell-bilist))
    (let(
          (c0 (left-neighbor-slot c1)) ; this will be tape, the list header
          )
      (insert-between c0 c1 new-cell)
      (to-rightmost c1)
      (to-leftmost new-cell)
      (values)
      ))
  (defun-typed -a<cell> ((c1 bilist-leftmost) (new-cell cell-bilist))
    (let(
          (c0 (left-neighbor-slot c1)) ; this will be tape, the list header
          )
      (insert-between c0 c1 new-cell)
      (to-interior c1)
      (to-leftmost new-cell)
      (values)
      ))
  ;;The solitary and leftmost cases must be handled by specialized implementation
  ;;functions.  We can't do it here because we don't know the relationship between
  ;;rightmost and the tape header for the implementation.
  (defun-typed -a<cell> ((c1 bilist-rightmost-interior) (new-cell cell))
    (let(
          (c0 (left-neighbor c1)) ; c1 is not leftmost, but c0 might be
          )
      (insert-between c0 c1 new-cell)
      (to-interior new-cell)
      (values)
      ))


  ;; Deletes the right neighbor cell.
  ;; This function is unable to make the tape empty.
  ;; This is here instead of in cell.lisp because c2 might be the tape header,
  ;; and other types might not have a link type tape header.
  ;;
    (defun-typed d<cell> ((cell bilist-leftmost) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          &allow-other-keys
          )
        ➜
        (let*(
               (c0 cell)
               (c1 (right-neighbor-slot c0)) ; this might be rightmost
               (c2 (right-neighbor-slot c1)) ; this might be the tape header
               )
          (extract c0 c1 c2)
          (when 
            (typep c1 'rightmost)
            (to-solitary c0)
            )
          (to-cell c1)
          [➜ok c1]
          )))
    (defun-typed d<cell> ((cell bilist-interior) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          &allow-other-keys
          )
        ➜
        (let*(
               (c0 cell)
               (c1 (right-neighbor-slot c0)) ; this might be rightmost
               (c2 (right-neighbor-slot c1)) ; this might be the tape header
               )
          (extract c0 c1 c2)
          (when 
            (typep c1 'rightmost)
            (to-rightmost c0)
            )
          (to-cell c1)
          [➜ok c1]
          )))

  ;; deletes the left neighbor cell
  ;; this function is unable to make the tape empty
  (defun-typed -d<cell> ((cell bilist-rightmost) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let*(
             (c2 cell)
             (c1 (left-neighbor-slot c2)) ; this might be leftmost
             (c0 (left-neighbor-slot c1)) ; this might be the tape header
             )
        (extract c0 c1 c2)
        (when 
          (typep c1 'leftmost)
          (to-solitary c2)
          )
        (to-cell c1)
        [➜ok c1]
        )))
  (defun-typed -d<cell> ((cell bilist-interior) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let*(
             (c2 cell)
             (c1 (left-neighbor-slot c2)) ; this might be leftmost
             (c0 (left-neighbor-slot c1)) ; this might be the tape header
             )
        (extract c0 c1 c2)
        (when 
          (typep c1 'leftmost)
          (to-leftmost c2)
          )
        (to-cell c1)
        [➜ok c1]
        )))

    (defun-typed d+<cell> ((cell bilist-leftmost-interior) &optional ➜)
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
