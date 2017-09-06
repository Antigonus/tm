#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Implementation of cell intended for use in a list.

Our version of the cons cell.

Contents is a single value, neighbors is a tape-array of neighbor linkes.  The contents can
be mulitplexed.  A neighbor link can be multiplexed.

Our tape-list is a doubly linked list.  This cell can be used to implement single linked lists
or trees, etc.

A cell with a subspace is simply a cell with an additional neighbor link for the subspace.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type definition
;;

  (def-type cell ()
    (
      ;; content can be multiplexed
      (content 
        :accessor content
        :initform ∅
        )

      ;; The cell neighbor slot is a tape-array of neighbors.  When we make a tape from
      ;; cells, each cell typically has two neighbors, a right neighbor and a left
      ;; neighbor.  The neighbors tape-array will not be multiplexed.  However, each
      ;; member neighbor link may be multiplexed.
      (neighbors
        :accessor neighbors
        :initform ∅
        )
      ))

  (defun-typed init ((cell cell) &optional ➜)
    (destructuring-bind
      (&key
        init-value
        (➜ok #'echo)
        ;;(➜fail (λ()(error 'bad-init-value)))
        &allow-other-keys
        )
      ➜
      (w<plex> (content cell) init-value ➜)
      [➜ok cell]
      ))

  (defparameter *right 0)
  (defparameter *left 1)


;;--------------------------------------------------------------------------------
;; read
;;
  (defun r<content> (cell &optional ➜)
    (r<plex> (content cell) ➜)
    )

  (defun r<neighbors> (cell &optional ➜)
    (r<tape-array> (neighbors cell)
      {
        :➜ok (λ(neighbor)(r<plex> neighbor ➜))
        (o ➜)
        }))

;;--------------------------------------------------------------------------------
;; write
;;
  (defmacro w<content> (cell instance &optional ➜)
    `(w<plex> (content ,cell) ,instance ,➜)
    )

  (defun w<neighbors> (cell a-neighbor-cell &optional ➜)
    (destructuring-bind
      (&key
        (address 0) ; selects the neighbor
        (channel (channel 0)) ; each neighbor may be multiplexed
        (➜ok (be t))
        (➜alloc-fail #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (declare (ignore ➜alloc-fail)) ; someday will have to fill this in ...
      (r<tape-array> (neighbors cell)
        {
          :address address

          :➜ok
          (λ(neighbor)
            (w<plex> neighbor a-neighbor-cell {:channel channel})
            (w<tape-array> (neighbors cell) neighbor {:address address})
            [➜ok]
            )

          :➜empty
          (λ()
            (let(neighbor)
              (w<plex> neighbor a-neighbor-cell {:channel channel})
              (w<tape-array> (neighbors cell) neighbor {:address address})
              [➜ok]
              ))
          })))

