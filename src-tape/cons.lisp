#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Init binds to a list, or creates a new list and shallow copies from another tape.

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
  (def-type cell-cons (cell)
    (
      (cons-cell ; will be a cons cell
        :initarg :cons-cell
        :accessor cons-cell
        )
      ))

  ;; these functions are tailored for this type
  ;; this would be a type class .. instead we make use of dispatch
  ;;(def-type tape-cons-functions (tape)
  ;;  )

  (def-type tape-cons (tape)
    (
      (cons-list ; will be a lisp list
        :initarg :cons-list
        :accessor cons-list
        )
      ))

  (def-type tape-cons-empty (tape-cons tape-empty)())
  (def-type tape-cons-active (tape-cons tape-active)())
  (defun-typed to-active ((tape tape-cons)) (change-class tape 'tape-cons-active))
  (defun-typed to-empty  ((tape tape-cons)) (change-class tape 'tape-cons-empty))

  ;; binds to a list
  (defun-typed init ((tape tape-cons) (init cons) &optional ➜)
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

  ;; shallow copies from another tape
  (defun-typed init ((tape-1 tape-cons) (tape-0 tape) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (to-empty tape-1)
      (shallow-copy-topo tape-1 tape-0)
      [➜ok tape-1]
      ))

  ;; provides list intialization for other tape types:
  (defun-typed init ((tape-1 tape) (a-list cons) &optional ➜)
    (mk 'tape-cons a-list
      {
        :➜ok (λ(tape-0) (init tape-1 tape-0 ➜))
        }))

;;--------------------------------------------------------------------------------
;; topology queries
;;
  (defun-typed =<cell> ((cell-0 cell-cons) (cell-1 cell-cons))
    (eq (cons-cell cell-0) (cons-cell cell-1))
    )

  (defun-typed r<cell> ((cell cell-cons)) (car (cons-cell cell)))
  (defun-typed w<cell> ((cell cell-cons) instance) (setf (car (cons-cell cell)) instance))

  (defun-typed leftmost ((tape tape-cons-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (make-instance 'cell-cons :cons-cell (cons-list tape))]
      ))

  (defun-typed right-neighbor ((cell cell-cons) &optional ➜)
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
          [➜ok (make-instance 'cell-cons :cons-cell (cdr (cons-cell cell)))]
          [➜rightmost]
          ))))

;;--------------------------------------------------------------------------------
;; topology manipulation
;;
  ;; accepts a tape-cons and a cell, makes the cell the new leftmost
  ;; will be a problem if (cons-list tape) is shared
  (defun-typed epa<cell> ((tape tape-cons-active) (cell cell-cons))
    (let(
          (cons-cell (cons-cell cell))
          )
      (rplacd cons-cell (cons-list tape))
      (setf (cons-list tape) cons-cell)
      ))
  (defun-typed epa<cell> ((tape tape-cons-empty) (cell cell-cons))
    (let(
          (cons-cell (cons-cell cell))
          )
      (rplacd cons-cell ∅)
      (setf (cons-list tape) cons-cell)
      ))

  ;; accepts a tape-cons and an instance, makes a new leftmost initialized with the
  ;; instance will be a problem if (cons-list tape) is shared
  (defun-typed epa<instance> ((tape tape-cons-active) instance)
    (let(
          (new-cons-cell (cons instance (cons-list tape)))
          )
    (setf (cons-list tape) new-cons-cell)
    ))
  (defun-typed epa<instance> ((tape tape-cons-empty) instance)
    (let(
          (new-cons-cell (cons instance ∅))
          )
      (setf (cons-list tape) new-cons-cell)
      (to-active tape)
      ))

  (defun-typed a<cell> ((cell-0 cell) (cell-1 cell))
    (let*(
           (cons0 (cons-cell cell-0))
           (cons1 (cons-cell cell-1))
           (cons2 (cdr cons0))
           )
      (rplacd cons1 cons2)
      (rplacd cons0 cons1)
      ))

  (defun-typed a<instance> ((cell-0 cell) instance)
    (let*(
           (cons0 (cons-cell cell-0))
           (cons1 (cons instance (cdr cons0)))
           )
      (rplacd cons0 cons1)
      ))

  ;; removes the leftmost cell and returns it
  ;; will be a problem if (cons-list tape) is shared
  (defun-typed epd<tape> ((tape tape-cons-active) &optional ➜)
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
        [➜ok (make-instance 'cell-cons :cons-cell leftmost)]
        )))

  ;; deletes the right neighbor cell
  (defun-typed d<cell> ((cell cell-cons) &optional ➜)
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
              [➜ok (make-instance 'cell-cons :cons-cell cell-1)]
              ))
          [➜rightmost]
          ))))

  ;; References to the right neghbor of leftmost get messed up,
  ;; but (cons-list tape) will be ok, so no tape sharing issues
  (defun-typed ◧d.<tape> ((tape tape-cons-active) &optional ➜)
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
              [➜ok (make-instance 'cell-cons :cons-cell cell-0)]
              ))
          })))

  (defun-typed epd*<tape> ((tape tape-cons) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (to-empty tape)
      (setf (cons-list tape) ∅) ; free the data
      [➜ok]
      ))

  (defun-typed d*<cell> ((cell cell-cons) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (rplacd (cons-cell cell) ∅)
      [➜ok]
      ))
