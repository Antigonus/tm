#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


Need to add in the no-alloc continuations

    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜bad (λ()(error 'bad-init-value)))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-type list-cell (cell)
    (
      (cons-cell ; will be a cons cell
        :initarg :cons-cell
        :accessor cons-cell
        )
      ))

  (def-type list-tape (tape)
    (
      (cons-list ; will be a lisp list
        :initarg :cons-list
        :accessor cons-list
        )
      ))

  ;; init value can not be ∅
  (defun-typed init ((tape list-tape) (init null) &optional ➜)
    (destructuring-bind
      (&key
        (➜bad (λ()(error 'bad-init-value)))
        &allow-other-keys
        )
      ➜
    [➜bad]
    )

  (defun-typed init ((tape list-tape) (init cons) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
    (setf (cons-list tape) init)
    [➜ok tape]
    )

  (defun-typed init ((tape list-tape) (seq sequence) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜bad (λ()(error 'bad-init-value)))
        &allow-other-keys
        )
      ➜
    (labels(
             (init-1 (i)
               (cond
                 ((≥ i (length seq))
                   ∅
                   )
                 (t
                   (cons (elt seq i) (init-1 (1+ i)))
                   )))
             )
      (cond
        ((= 0 (length seq))
          [➜bad]
          )
        (t
          (setf (cons-list tape) (init-1 0))
          [➜ok tape]
          ))
      )))

  (defun-typed init ((tape-1 list-tape) (tape-0 tape) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      (labels(
               (init-1 (cell)
                 (cond
                   ((¬ cell)
                     ∅
                     )
                   (t
                     (cons (read cell) (init-1 (right-neighbor cell)))
                     )))
               (init-0 ()
                 (let(
                       (cell-0 (leftmost tape-0))
                       )
                   (cons (read cell-0) (init-1 (right-neighbor cell-0)))
                   ))
               )
        (setf (cons-list tape-1) (init-0))
        [➜ok tape-1]
        ))

;;--------------------------------------------------------------------------------
;; accessing instances
;;
  (defun-typed e-s*r<tape> ((tape list-tape))
    (car (cons-list tape))
    )

  (defun-typed e-s*sr ((tape list-tape) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ()(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (let(
            (leftmost (cons-list tape))
            )
        (if
          (cdr leftmost)
          [➜ok (cadr leftmost)]
          [➜rightmost]
          ))))

  (defun-typed e-s*w<tape> ((tape list-tape) instance)
    (setf (car (cons-list tape)) instance)
    )

  (defun-typed e-s*sw ((tape list-tape) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (let(
            (leftmost (cons-list tape))
            )
        (if
          (cdr leftmost)
          (progn
            (setf (cadr leftmost) instance)
            [➜ok]
            )
          [➜rightmost]
          )))

;;--------------------------------------------------------------------------------
;; topology queries
;;
  (defun-typed r<cell> ((cell list-cell)) (car (cons-cell cell)))
  (defun-typed w<cell> ((cell list-cell) instance) (setf (car (cons-cell cell)) instance))

  (defun-typed leftmost ((tape tape-list))
    (make-instance 'list-cell :cell (cons-list tape))
    )
  (defun-typed right-neighbor ((cell list-cell) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (let(
            (rn (cdr (cons-cell cell)))
            )
        (if rn
          [➜ok (make-instance 'list-cell :cell (cdr (cons-cell cell)))]
          [➜rightmost]
          ))))

;;--------------------------------------------------------------------------------
;; topology manipulation
;;
  ;; accepts a list-tape and a cell, makes the cell the new leftmost
  ;; will be a problem if (cons-list tape) is shared
  (defun-typed epa<cell> ((tape list-tape) (cell cell))
    (let(
          (cons-cell (cons-cell cell))
          )
      (replacd cons-cell (cons-list tape))
      (setf (cons-list tape) cons-cell)
      ))

  ;; accepts a list-tape and an instance, makes a new leftmost initialized with the instance
  ;; will be a problem if (cons-list tape) is shared
  (defun-typed epa<instance> ((tape list-tape) instance)
    (setf (cons-list tape) (cons instance (tape tm)))
    )

  ;; deletes the leftmost cell
  ;; will be a problem if (cons-list tape) is shared
  (defun-typed epd<cell> ((tape list-tape))
    (let(
          (leftmost (cons-list tape))
          )
      (setf (cons-list tape) (cdr leftmost))
      (make-instance 'list-cell :cell leftmost)
      ))

  ;; deletes the right neighbor cell
  (defun-typed d<cell> ((cell list-cell))
    (let*(
           (cell-0 (cons-cell cell))
           (cell-1 (cdr cell-0)) ; this is the right neighbor
           (cell-2 (cdr cell-1))
           )
      (replacd cell-0 cell-2) ; this orphans cell-1
      (make-instance 'list-cell :cell cell-1)
      ))

  ;; d. swaps cell-1's instance with cell-0's before the delete, thus creating the
  ;; appeareance of deleting cell-0.  As cell's are intended to be the targets of
  ;; references, any cell-0 or cell-1 references would have to be 'fixed' after this
  ;; operation.
  (defun-typed d.<cell> ((cell list-cell))
    (let*(
           (cell-0 (cons-cell cell))
           (cell-0-instance (car cell-0))
           (cell-1 (cdr cell-0)) ; this is the right neighbor
           (cell-1-instance (car cell-1))
           (cell-2 (cdr cell-1))
           )
      (setf (car cell-0) cell-1-instance)
      (setf (car cell-1) cell-0-instance)
      (replacd cell-0 cell-2) ; this orphans cell-1
      (make-instance 'list-cell :cell cell-1)
      ))

  ;; References to the rightneghbor of leftmost get messed up,
  ;; but (cons-list tape) will be ok, so no tape sharing issues
  (defun-typed e-s*d.<cell> ((tape list-tape))
    (let(
          (leftmost (cons-list tape))
          )
      (d.<cell> leftmost)
      ))
