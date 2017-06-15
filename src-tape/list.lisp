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
  (def-type cell-list (cell)
    (
      (cons-cell ; will be a cons cell
        :initarg :cons-cell
        :accessor cons-cell
        )
      ))

  (def-type tape-list (tape)
    (
      (cons-list ; will be a lisp list
        :initarg :cons-list
        :accessor cons-list
        )
      ))
  (def-type tape-list-empty (tape-list tape-empty)())
  (def-type tape-list-active (tape-list tape-active)())
  (defun-typed to-active ((tape tape-list)) (change-class tape 'tape-list-active))
  (defun-typed to-empty  ((tape tape-list)) (change-class tape 'tape-list-empty))

  ;; binds to a list
  (defun-typed init ((tape tape-list) (init cons) &optional ➜)
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

  ;; provides list intialization for other tape types:
  (defun-typed init ((tape-1 tape) (a-list cons) &optional ➜)
    (mk 'tape-list a-list
      {
        :➜ok (λ(tape-0) (init tape-1 tape-0 ➜))
        }))


  ;; shallow copies from another tape
  (defun-typed init ((tape-1 tape-list) (tape-0 tape-active) &optional ➜)
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

;;--------------------------------------------------------------------------------
;; topology queries
;;
  (defun-typed =<cell> ((cell-0 cell-list) (cell-1 cell-list))
    (eq (cons-cell cell-0) (cons-cell cell-1))
    )

  (defun-typed r<cell> ((cell cell-list)) (car (cons-cell cell)))
  (defun-typed w<cell> ((cell cell-list) instance) (setf (car (cons-cell cell)) instance))

  (defun-typed leftmost ((tape tape-list-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (make-instance 'cell-list :cons-cell (cons-list tape))]
      ))

  (defun-typed right-neighbor ((cell cell-list) &optional ➜)
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
          [➜ok (make-instance 'cell-list :cons-cell (cdr (cons-cell cell)))]
          [➜rightmost]
          ))))

;;--------------------------------------------------------------------------------
;; topology manipulation
;;
  ;; accepts a tape-list and a cell, makes the cell the new leftmost
  ;; will be a problem if (cons-list tape) is shared
  (defun-typed epa<cell> ((tape tape-list-active) (cell cell-list))
    (let(
          (cons-cell (cons-cell cell))
          )
      (rplacd cons-cell (cons-list tape))
      (setf (cons-list tape) cons-cell)
      ))
  (defun-typed epa<cell> ((tape tape-list-empty) (cell cell-list))
    (let(
          (cons-cell (cons-cell cell))
          )
      (rplacd cons-cell ∅)
      (setf (cons-list tape) cons-cell)
      ))

  ;; accepts a tape-list and an instance, makes a new leftmost initialized with the
  ;; instance will be a problem if (cons-list tape) is shared
  (defun-typed epa<instance> ((tape tape-list-active) instance)
    (let(
          (new-cons-cell (cons instance (cons-list tape)))
          )
    (setf (cons-list tape) new-cons-cell)
    ))
  (defun-typed epa<instance> ((tape tape-list-empty) instance)
    (let(
          (new-cons-cell (cons instance ∅))
          )
      (setf (cons-list tape) new-cons-cell)
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
  (defun-typed epd<tape> ((tape tape-list-active) &optional ➜)
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
        [➜ok (make-instance 'cell-list :cons-cell leftmost)]
        )))

  ;; deletes the right neighbor cell
  (defun-typed d<cell> ((cell cell-list) &optional ➜)
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
              [➜ok (make-instance 'cell-list :cons-cell cell-1)]
              ))
          [➜rightmost]
          ))))

  ;; References to the right neghbor of leftmost get messed up,
  ;; but (cons-list tape) will be ok, so no tape sharing issues
  (defun-typed e-s*d.<tape> ((tape tape-list-active) &optional ➜)
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
              [➜ok (make-instance 'cell-list :cons-cell cell-0)]
              ))
          })))

  (defun-typed epd*<tape> ((tape tape-list) &optional ➜)
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

  (defun-typed d*<cell> ((cell cell-list) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (rplacd (cons-cell cell) ∅)
      [➜ok]
      ))
