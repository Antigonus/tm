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
  (def-type bilink ()
    (
      (right-neighbor
        :initarg right-neighbor
        :accessor right-neighbor
        )
      (left-neighbor
        :initarg left-neighbor
        :accessor left-neighbor
        )
      ))

  (def-type cell-bilist (bilink cell)
    (
      (contents :initarg contents :accessor contents)
      ))
  (def-type cell-bilist-leftmost (cell-bilist cell-leftmost)())
  (def-type cell-bilist-rightmost (cell-bilist cell-rightmost)())
  (def-type cell-bilist-singleton (cell-bilist-rightmost cell-bilist-leftmost cell-singleton)())

  (defun-typed to-cell ((cell cell-bilist))(change-class cell 'cell-bilist))
  (defun-typed to-leftmost ((cell cell-bilist))(change-class cell 'cell-bilist-leftmost))
  (defun-typed to-rightmost ((cell cell-bilist))(change-class cell 'cell-bilist-rightmost))
  (defun-typed to-singleton ((cell cell-bilist))(change-class cell 'cell-bilist-singleton))


;;--------------------------------------------------------------------------------
;; queries
;;
  (defun-typed r<cell> ((cell cell-bilist)) (contents cell))
  (defun-typed w<cell> ((cell cell-bilist) instance) (setf (contents cell) instance))

;;--------------------------------------------------------------------------------
;; topology manipulation
;;

  ;; When we use self-pointers instead of nil to terminate ends, insert
  ;; will work even against a single cell.
  (defun insert<cell> (c0 c1 new-cell)
    (setf (left-neighbor new-cell) c0)
    (setf (right-neighbor new-cell) c1)
    (setf (left-neighbor c1) new-cell)
    (setf (right-neighbor c0) new-cell)
    )

  (defun-typed a<cell> ((c0 cell-bilist-singleton) (new-cell cell-bilist))
    (let(
          (c1 (right-neighbor c0)) ; this will be tape, the list header
          )
      (insert<cell> c0 c1 new-cell)
      (to-leftmost c0)
      (to-rightmost new-cell)
      (values)
      ))
  (defun-typed a<cell> ((c0 cell-bilist-rightmost) (new-cell cell-bilist))
    (let(
          (c1 (right-neighbor c0)) ; this will be tape, the list header
          )
      (insert<cell> c0 c1 new-cell)
      (to-cell c0)
      (to-rightmost new-cell)
      (values)
      ))
  (defun-typed a<cell> ((c0 cell-bilist) (new-cell cell-bilist))
    (let(
          (c1 (right-neighbor c0)) ; this will be tape, the list header
          )
      (insert<cell> c0 c1 new-cell)
      (values)
      ))

  (defun-typed -a<cell> ((c1 cell-bilist-singleton) (new-cell cell-bilist))
    (let(
          (c0 (left-neighbor c1)) ; this will be tape, the list header
          )
      (insert<cell> c0 c1 new-cell)
      (to-rightmost c0)
      (to-leftmost new-cell)
      (values)
      ))

  (defun-typed -a<instance> ((c0 cell-bilist) instance)
    (let(
          (new-cell (make-instance 'cell-bilist :contents instance))
          )
      (insert<cell> (left-neighbor c0) c0 new-cell)
      ))

  ;; for internal use
  ;; removes c1
  (defun remove<cell> (c0 c1 c2)
    (setf (left-neighbor c2) c0)
    (setf (right-neighbor c0) c2)
    (setf (left-neighbor c1) c1) ; so that c1 can not prevent neighbors from being gc'ed
    (setf (right-neighbor c1) c1)
    (to-cell c1)
    )

  ;; deletes the right neighbor cell
  ;; this function is unable to make the tape empty
  ;; the case where cell is rightmost is handled generically in cell.lisp
  (defun-typed d<cell> ((cell cell-bilist) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let*(
             (c0 cell)
             (c1 (right-neighbor c0)) ; this might be rightmost
             (c2 (right-neighbor c1)) ; this might be the tape header
             )
        (remove<cell> c0 c1 c2)
        [➜ok c1]
        )))

  ;; deletes the left neighbor cell
  ;; this function is unable to make the tape empty
  ;; the case where cell is leftmost is handled generically in cell.lisp
  (defun-typed -d<cell> ((cell cell-bilist) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let*(
             (c2 cell)
             (c1 (left-neighbor c2)) ; this might be leftmost
             (c0 (left-neighbor c1)) ; this might be the tape header
             )
        (remove<cell> c0 c1 c2)
        [➜ok c1]
        )))

  (defun-typed d*<cell> ((cell cell-bilist) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let(
            (rn (right-neighbor cell))
            )
        (setf (left-neighbor rn) rn)
        (setf (right-neighbor cell) cell)
        [➜ok rn]
        )))
