#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Implementation of cell intended for use in a list.

CLOS version of the cons cell.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type definition
;;
  (def-type link ()
    (
      (right-neighbor
        :initarg :right-neighbor
        :accessor right-neighbor
        )
      ))
  ;; though a list cell only has one link, the header for a list tape keeps track of both
  ;; tape boundaries
  (def-type bilink (link)
    (
      (left-neighbor
        :initarg :left-neighbor
        :accessor left-neighbor
        )
      ))

  (defparameter *direction-right* 0)
  (defparameter *direction-left*  1)

  (def-type cell-list (link cell substrate)
    (
      (contents :initarg :contents :accessor contents)
      ))

  ;; these are only used as typed function argument specifiers
  ;;
    (def-type list-leftmost-interior (cell-list leftmost-interior)())
    (def-type list-rightmost-interior (cell-list rightmost-interior)())

  ;; all cells appearing on a tape have exactly one of these subtypes
  ;;
    (def-type list-interior  (list-leftmost-interior list-rightmost-interior interior)())
    (def-type list-leftmost  (list-leftmost-interior leftmost)())
    (def-type list-rightmost (list-rightmost-interior rightmost)())
    (def-type list-solitary  (list-leftmost list-rightmost solitary)())
    
  (defun-typed to-cell      ((cell cell-list))(change-class cell 'cell-list))
  (defun-typed to-interior  ((cell cell-list))(change-class cell 'list-interior))
  (defun-typed to-leftmost  ((cell cell-list))(change-class cell 'list-leftmost))
  (defun-typed to-rightmost ((cell cell-list))(change-class cell 'list-rightmost))
  (defun-typed to-solitary  ((cell cell-list))(change-class cell 'list-solitary))


  (defun-typed init ((cell cell-list) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜fail (λ()(error 'bad-init-value)))
        status right-neighbor
        )
      ➜
      (w cell instance)
      (cond
        (right-neighbor (setf (right-neighbor cell) right-neighbor))
        (t              (setf (right-neighbor cell) cell))
        )
      (when status
        (case status
          (interior  (to-interior  cell))
          (leftmost  (to-leftmost  cell))
          (rightmost (to-rightmost cell))
          (solitary  (to-solitary  cell))
          (otherwise (return-from init [➜fail]))
          ))
      [➜ok cell]
      ))

;;--------------------------------------------------------------------------------
;; cell functions
;;
  ;; we don't provide a cell copy, so one can usually just use eq
  (defun-typed =<cell> ((cell-0 cell-list) (cell-1 cell-list) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      (if
        (∧
          (eq (right-neighbor cell-0) (right-neighbor cell-1))
          (eq (contents cell-0) (contents cell-1))
          )
        [➜t]
        [➜∅]
        )))

  (defun-typed r ((cell cell-list) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (contents cell)]
      ))
 
  (defun-typed w ((cell cell-list) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (contents cell) instance)
      [➜ok]
      ))


;;--------------------------------------------------------------------------------
;; cell neighbor functions
;;
  ;; by contract distance is positive when neighbor-1 is called
  ;; differential is a function
  (defun neighbor-1 (cell differential n &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ(cell n)(declare (ignore cell n))(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (⟳(λ(➜again)
          (cond
            ((typep cell 'rightmost) [➜rightmost cell n])
            ((= n 0) [➜ok cell])
            (t
              (setf cell [differential cell])
              (decf n)
              [➜again]
              ))))))

  (defun-typed neighbor ((cell cell-list) &optional ➜)
    (destructuring-bind
      (&key
        (➜bad-direction (λ()(error 'bad-direction)))
        (distance 1)
        &allow-other-keys
        )
      ➜
      (cond
        ((< distance 0) [➜bad-direction])
        (t 
          (neighbor-1 cell #'right-neighbor distance ➜)
          ))))

;;--------------------------------------------------------------------------------
;; topology manipulation
;;
  (def-function-class connect (c0 c1))
  (defun-typed connect ((c0 cell-list) (c1 cell-list))
    (setf (right-neighbor c0) c1)
    )

  (defun-typed a<cell> ((c0 list-solitary) (new-cell cell-list))
    (connect c0 new-cell)
    (to-leftmost c0)
    (to-rightmost new-cell)
    t
    )
  (defun-typed a<cell> ((c0 list-rightmost) (new-cell cell-list))
    (connect c0 new-cell)
    (to-interior c0)
    (to-rightmost new-cell)
    t
    )
  (defun-typed a<cell> ((c0 leftmost-interior) (new-cell cell))
    (connect c0 new-cell)
    (to-interior new-cell)
    t
    )

  ;; Deletes the right neighbor cell.
  ;;
    (def-function-class extract (c0 c1))
    (defun-typed extract ((c0 list-leftmost) (c1 list-rightmost))
      (setf (right-neighbor c0) c0)
      (setf (right-neighbor c1) c1)
      (to-solitary c0)
      (to-cell c1)
      )
    (defun-typed extract ((c0 list-interior) (c1 list-rightmost))
      (setf (right-neighbor c0) c0)
      (setf (right-neighbor c1) c1)
      (to-rightmost c0)
      (to-cell c1)
      )
    (defun-typed extract ((c0 list-interior) (c1 list-interior))
      (let(
            (c2 (right-neighbor c1))
            )
        (setf (right-neighbor c0) c2)
        (setf (right-neighbor c1) c1)
        (to-cell c1)
        ))

    ;; rightmost and solitary handled on the interface
    (defun-typed d<cell> ((c0 list-leftmost-interior) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          &allow-other-keys
          )
        ➜
        (let(
              (c1 (right-neighbor c0))
              )
          (extract c0 c1)
          [➜ok c1]
          )))

    (def-function-class disconnect (c0 c1))
    (defun-typed disconnect ((c0 list-leftmost) (c1 list-rightmost-interior))
      (setf (right-neighbor c0) c0)
      (to-solitary c0)
      )
    (defun-typed disconnect ((c0 list-interior) (c1 list-rightmost-interior))
      (setf (right-neighbor c0) c0)
      (to-rightmost c0)
      )

    ;; c0 solitary and rightmost cases handled on the interface
    (defun-typed d+<cell> ((c0 list-leftmost-interior) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          &allow-other-keys
          )
        ➜
        (let(
              (c1 (right-neighbor c0))
              )
          (disconnect c0 c1)
          [➜ok c1]
          )))
