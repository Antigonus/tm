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

  (defparameter *right* 0)

  (def-type cell-list (link cell substrate)
    (
      (contents :initarg :contents :accessor contents)
      ))

  (defun-typed init ((cell cell-list) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜fail (λ()(error 'bad-init-value)))
        right-neighbor
        &allow-other-keys
        )
      ➜
      (w cell instance)
      (cond
        (right-neighbor (setf (right-neighbor cell) right-neighbor))
        (t              (setf (right-neighbor cell) ∅))
        )
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

  (defun-typed r<cell> ((cell cell-list) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (contents cell)]
      ))
 
  (defun-typed w<cell> ((cell cell-list) instance &optional ➜)
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
        (➜right-bound (λ(cell n)(declare (ignore cell n))(error 'step-from-right-bound)))
        &allow-other-keys
        )
      ➜
      (⟳(λ(➜again)
          (cond
            ((= n 0) [➜ok cell])
            ((typep cell 'right-bound) [➜right-bound cell n])
            (t
              (setf cell [differential cell])
              (decf n)
              [➜again]
              ))))))

  (defun-typed neighbor ((cell cell-list) &optional ➜)
    (destructuring-bind
      (&key
        (➜bad-direction (λ()(error 'bad-direction)))
        (n 0)
        &allow-other-keys
        )
      ➜
      (cond
        ((≥ n 0)
          (neighbor-1 cell #'right-neighbor n ➜)
          )
        (t [➜bad-direction])
        )))

;;--------------------------------------------------------------------------------
;; primitive topology manipulation
;;
  ;; terminate the connections of a cell so that it can not affect gc
  ;;
    (defun cap-right (c)
      (setf (right-neighbor c) ∅)
      (to-right-bound c)
      )

    (def-function-class cap-left (c))
    (defun-typed cap-left ((c cell-list))
      (to-left-bound c)
      )

    (def-function-class cap (c))
    (defun-typed cap ((c cell-list)) 
      (setf (right-neighbor c) ∅)
      (to-cell c)
      )

  (def-function-class cap<tape> (tape))
  (defun-typed cap<tape> ((tape cell-list)) 
    (setf (right-neighbor tape) ∅)
    (to-empty tape)
    )

  ;; c0 and c1 are two cells to be connected
  ;;
    (def-function-class connect (c0 c1))
    (defun-typed connect ((c0 link)(c1 link))
      (setf (right-neighbor c0) c1)
      )

  ;; c0 and c1 are neighbors.  re-routes connections around cell c1
  ;;
    (def-function-class extract (c0 c1))
    (defun-typed extract ((c0 list-left-bound) (c1 list-right-bound))
      (cap-right c0)
      (to-solitary c0)
      (cap c1)
      )
    (defun-typed extract ((c0 list-interior) (c1 list-right-bound))
      (cap-right c0)
      (cap c1)
      )
    (defun-typed extract ((c0 list-left-bound-interior) (c1 list-interior))
      (let(
            (c2 (right-neighbor c1))
            )
        (connect c0 c2)
        ;; c0 and c2 each maintains its status
        (cap c1)
        ))

  ;; c0 and c1 are neighbors. disconnects them.
  ;;
    (def-function-class disconnect (c0 c1))
    (defun-typed disconnect ((c0 list-left-bound) (c1 list-right-bound-interior))
      (cap-right c0)
      (to-solitary c0)
      (cap-left c1)
      )
    (defun-typed disconnect ((c0 list-interior) (c1 list-right-bound-interior))
      (cap-right c0)
      (cap-left c1)
      )

;;--------------------------------------------------------------------------------
;; topology manipulation, interface implementations
;;
  ;; c0 and c2 are neighbors, inserts c1 between them
  ;;
    (defun-typed a<cell> ((c0 list-solitary) (c1 cell-list) &optional ➜)
      (declare (ignore ➜))
      (cap-right c1)
      (connect c0 c1)
      (to-left-bound c0)
      )
    (defun-typed a<cell> ((c0 list-right-bound) (c1 cell-list)  &optional ➜)
      (declare (ignore ➜))
      (cap-right c1)
      (connect c0 c1)
      (to-interior c0)
      )
    (defun-typed a<cell> ((c0 list-left-bound-interior) (c1 cell-list)  &optional ➜)
      (declare (ignore ➜))
      (let(
            (c2 (right-neighbor c0))
            )
        (connect c0 c1)
        (connect c1 c2)
        (to-interior c1)
        ;; c0 keeps its status
      ))

  ;; Deletes the right neighbor cell.
  ;; right-bound handled on the interface.
  ;; solitary, though this has the same behavior for all cells, had to be put 
  ;; here as otherwise CLOS wold choose list-left-bound-interior as being more specific
  ;;
    (defun-typed d<cell> ((cell list-solitary) &optional ➜)
      (destructuring-bind
        (&key
          (➜right-bound (λ()(error 'dealloc-on-right-bound)))
          &allow-other-keys
          )
        ➜
        [➜right-bound]
        ))
    (defun-typed d<cell> ((c0 list-left-bound-interior) &optional ➜)
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

  ;; c0 right-bound case handled on the interface.
  ;; solitary, though this has the same behavior for all cells, had to be put 
  ;; here as otherwise CLOS wold choose list-left-bound-interior as being more specific
  ;;
    (defun-typed d+<cell> ((cell list-solitary) &optional ➜)
      (destructuring-bind
        (&key
          (➜right-bound (λ()(error 'dealloc-on-right-bound)))
          &allow-other-keys
          )
        ➜
        [➜right-bound]
        ))
    (defun-typed d+<cell> ((c0 list-left-bound-interior) &optional ➜)
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
