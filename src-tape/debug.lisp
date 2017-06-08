#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|# 

;; if the instances in two cells are equal
(def-function-class =<cell><instances> (cell-0 cell-1))
(defun-typed =<cell><instances> ((cell-0 cell) (cell-1 cell))
  (equal (read cell-0) (read cell-1))
  )

;; if two tapes have equivalent cells
;; for tape machines use universal quanitification with a predicate instead of this.
;; equiv is a function that is given two cells, if desired, inside of equiv read those
;; cells to to get an instance by instance comparison.
(def-function-class =<tape> (tape-0 tape-1 &optional equiv))

(defun-typed =<tape> ((tape-0 null) (tape-1 null) &optional equiv)
  (declare (ignore equiv)) ; equiv takes cell parameters
  t
  )
(defun-typed =<tape> ((tape-0 tape) (tape-1 null) &optional equiv)
  (declare (ignore equiv)) ; equiv takes cell parameters
  ∅
  )
(defun-typed =<tape> ((tape-0 null) (tape-1 tape) &optional equiv)
  (declare (ignore equiv)) ; equiv takes cell parameters
  ∅
  )
(defun-typed =<tape>
  (
    (tape-0 tape) 
    (tape-1 tape)
    &optional
    (equiv #'=<cell><instances>)
    )
  (lables(
           (=tape-0 ()
             (let(
                   (cell-0 (leftmost tape-0))
                   (cell-1 (leftmost tape-1))
                   )
               (=tape-1 cell-0 cell-1)
               ))
           (=tape-1 (cell-0 cell-1)
             (∨
               (∧
                 cell-0
                 cell-1
                 [equiv cell-0 cell-1]
                 (=right-hand-side cell-0 cell-1)
                 )
               (∧
                 (¬ cell-0)
                 (¬ cell-1)
                 ))))
    (=right-hand-side (cell-0 cell-1)
      (let(
            (next-cell-0 (right-neighbor cell-0))
            (next-cell-1 (right-neighbor cell-1))
            )
        (=tape-1 next-cell-0 next-cell-1)
        )
      )
    (=tape-0())
    ))

(def-function-class print<cell> (cell))
(defun-typed print<cell> ((cell cell)) (princ (read cell)))

(def-function-class print-tape (tape))
(defun-typed print<tape> ((tape list-tape))
  (lables(
           (print-0 ()
             (print-1 (leftmost tape))
             )
           (print-1 (cell)
             (when cell
               (print<cell> cell)
               (print-2 (right-neighbor cell))
               ))
           (print-2 (cell)
             (when cell
               (princ " ")
               (print-cell cell)
               (print-2 (right-neighbor cell))
               ))
           )
    (print-0)
    ))


