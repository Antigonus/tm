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
  (def-type list-tape-active (list-tape tape-active)())
  (def-type list-tape-empty (list-tape tape-empty)())
  (defun-typed to-active ((tape list-tape)) (change-class tape 'list-tape-active))
  (defun-typed to-empty  ((tape list-tape)) (change-class tape 'list-tape-empty))

  (defun-typed init ((tape list-tape) (init cons) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (setf (cons-list tape) init)
      (to-active tape)
      [➜ok tape]
      ))

  ;; called via call-next-method after sequence is checked not to be zero length
  (defun-typed init ((tape list-tape) (seq sequence) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
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
        ((∨ (¬ seq) (= 0 (length seq)))
          (to-empty tape)
          [➜ok tape]
          )
        (t
          (setf (cons-list tape) (init-1 0))
          (to-active tape)
          [➜ok tape]
          )))))

  (defun-typed init ((tape-1 list-tape) (tape-0 tape-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (labels(
               (init-1 (cell) ; given a cell, returns a cons cell for the right-neighbor
                 (right-neighbor cell
                   {
                     :➜ok 
                     (λ(the-right-neighbor)
                       (cons (r<cell> the-right-neighbor) (init-1 the-right-neighbor))
                       )
                     :➜rightmost
                     (be ∅)
                     }))
               (init-0 () ; returns a first cons cell
                 (let(
                       (cell-0 (leftmost tape-0))
                       )
                   (cons (r<cell> cell-0) (init-1 cell-0))
                   ))
               )
        (setf (cons-list tape-1) (init-0))
        (to-active tape-1)
        [➜ok tape-1]
        )))

;;--------------------------------------------------------------------------------
;; accessing instances
;;
  (defun-typed e-s*r ((tape list-tape-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (car (cons-list tape))]
      ))

  (defun-typed e-s*sr ((tape list-tape-active) &optional ➜)
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

  (defun-typed e-s*w ((tape list-tape-active) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (car (cons-list tape)) instance)
      [➜ok]
      ))

  (defun-typed e-s*sw ((tape list-tape-active) instance &optional ➜)
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
          ))))

;;--------------------------------------------------------------------------------
;; topology queries
;;
  (defun-typed =<cell> ((cell-0 list-cell) (cell-1 list-cell))
    (eq (cons-cell cell-0) (cons-cell cell-1))
    )

  (defun-typed r<cell> ((cell list-cell)) (car (cons-cell cell)))
  (defun-typed w<cell> ((cell list-cell) instance) (setf (car (cons-cell cell)) instance))

  (defun-typed leftmost ((tape list-tape-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (make-instance 'list-cell :cons-cell (cons-list tape))]
      ))

  (defun-typed right-neighbor ((cell list-cell) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ()(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (let(
            (rn (cdr (cons-cell cell)))
            )
        (if rn
          [➜ok (make-instance 'list-cell :cons-cell (cdr (cons-cell cell)))]
          [➜rightmost]
          ))))

;;--------------------------------------------------------------------------------
;; topology manipulation
;;
  ;; accepts a list-tape and a cell, makes the cell the new leftmost
  ;; will be a problem if (cons-list tape) is shared
  (defun-typed epa<cell> ((tape list-tape-active) (cell list-cell))
    (let(
          (cons-cell (cons-cell cell))
          )
      (rplacd cons-cell (cons-list tape))
      (setf (cons-list tape) cons-cell)
      ))
  (defun-typed epa<cell> ((tape list-tape-empty) (cell list-cell))
    (let(
          (cons-cell (cons-cell cell))
          )
      (rplacd cons-cell ∅)
      (setf (cons-list tape) cons-cell)
      ))

  ;; accepts a list-tape and an instance, makes a new leftmost initialized with the instance
  ;; will be a problem if (cons-list tape) is shared
  (defun-typed epa<instance> ((tape list-tape-active) instance)
    (let(
          (new-cons-cell (cons instance (cons-list tape)))
          )
    (setf (cons-list tape) new-cons-cell)
    ))
  (defun-typed epa<instance> ((tape list-tape-empty) instance)
    (let(
          (new-cons-cell (cons instance ∅))
          )
      (setf (cons-list tape) new-cons-cell)
      ))

  ;; removes the leftmost cell and returns it
  ;; will be a problem if (cons-list tape) is shared
  (defun-typed epd<tape> ((tape list-tape-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let*(
             (leftmost (cons-list tape))
             (right-neighbor (cdr leftmost))
             )
        (if right-neighbor
          (setf (cons-list tape) right-neighbor)
          (progn
            (setf (cons-list tape) ∅)
            (to-empty tape)
            ))
        [➜ok (make-instance 'list-cell :cons-cell leftmost)]
        )))

  ;; deletes the right neighbor cell
  (defun-typed d<cell> ((cell list-cell) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ()(error 'dealloc-on-rightmost)))
        &allow-other-keys
        )
      ➜
      (let*(
             (cell-0 (cons-cell cell))
             (cell-1 (cdr cell-0)) ; this is the right neighbor
             )
        (if cell-1
          (progn
            (let(
                  (cell-2 (cdr cell-1))
                  )
              (rplacd cell-0 cell-2) ; this orphans cell-1
              [➜ok (make-instance 'list-cell :cons-cell cell-1)]
              ))
          [➜rightmost]
          ))))

  ;; References to the right neghbor of leftmost get messed up,
  ;; but (cons-list tape) will be ok, so no tape sharing issues
  (defun-typed e-s*d.<tape> ((tape list-tape-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (d.<cell> (leftmost tape)
        {
          :➜ok ➜ok
          :➜rightmost
          (λ()
            (let(
                  (cell-0 (cons-list tape))
                  )
              (setf (cons-list tape) ∅)
              (to-empty tape)
              [➜ok (make-instance 'list-cell :cons-cell cell-0)]
              ))
          })))
