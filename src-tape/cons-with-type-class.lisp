#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Init binds to a Lisp list, or creates a new Lisp list and shallow copies from another tape.

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
;; type
;;
  (def-type cell-cons (cell)
    (
      (cons-cell ; will be a cons cell
        :initarg :cons-cell
        :accessor cons-cell
        )
      ))

  (def-type tape-cons-class (tape)
    (
      (cons-list ; will be a lisp list
        :initarg :cons-list
        :accessor cons-list
        )
      ))

  (def-type tape-cons-class-empty (tape-cons-class tape-empty)())
  (def-type tape-cons-class-active (tape-cons-class tape-active)())
  (defun-typed to-active ((tape tape-cons-class)) (change-class tape 'tape-cons-class-active))
  (defun-typed to-empty  ((tape tape-cons-class)) (change-class tape 'tape-cons-class-empty))

  ;; these functions are tailored for this type
  ;; this would be a type class .. instead we make use of dispatch
  ;; (def-type tape-cons-class-functions (tape-functions)
  ;;   )

;;--------------------------------------------------------------------------------
;; init
;;
  ;;--------------------------------------------------------------------------------
  ;; type class functions
  ;; sets the instance specific and type specific functions
    (defun-typed mk-type-class-functions ((tape tape-cons-class))
      (setf (=<cell> tape) #'tape-cons-class-=<cell>)
      (setf (r<cell> tape) #'tape-cons-class-r<cell>)
      (setf (w<cell> tape) #'tape-cons-class-w<cell>)
      (setf (right-neighbor tape) #'tape-cons-class-right-neighbor)
      (setf (a<cell> tape) #'tape-cons-class-a<cell>)
      (setf (a<instance> tape) #'tape-cons-class-a<instance>)
      (setf (d<cell> tape) #'tape-cons-class-d<cell>)
      (setf (d.<cell> tape) ;(*1)
        (λ(cell &optional ➜)
          [(tape-class-d.<cell> *tape-class*) tape cell ➜]
          ))
      (setf (d*<cell> tape) #'tape-cons-class-d*<cell>)
      )
    ;; (*1) This is the only context using function in this type class.
    ;; tape-class-d.<cell> is a generic function, it is 'inherited' from tape.
    ;; *tape-class* exists when mk-type-function is called, so the lookup of d.<cell> can
    ;; be curried out.  Tape also exists when mk-type-class-functions is called, so Lisp
    ;; also has the option of compiling tape into the d.<cell> function.

  ;; binds to a list
  (defun-typed init ((tape tape-cons-class) (init cons) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (setf (cons-list tape) init)
      (to-active tape)
      (mk-type-class-functions tape)
      [➜ok tape]
      ))

  ;; shallow copies from another tape
  (defun-typed init ((tape-1 tape-cons-class) (tape-0 tape) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (to-empty tape-1)
      (shallow-copy-topo tape-1 tape-0)
      (mk-type-class-functions tape-1)
      [➜ok tape-1]
      ))

  ;; provides list intialization for other tape types:
  (defun-typed init ((tape-1 tape) (a-list cons) &optional ➜)
    (mk 'tape-cons-class a-list
      {
        :➜ok (λ(tape-0) (init tape-1 tape-0 ➜))
        }))


;;--------------------------------------------------------------------------------
;; topology queries
;;
  (defun tape-cons-class-=<cell> (cell-0 cell-1)
    (eq (cons-cell cell-0) (cons-cell cell-1))
    )

  (defun tape-cons-class-r<cell> (cell) (car (cons-cell cell)))
  (defun tape-cons-class-w<cell> (cell instance) (setf (car (cons-cell cell)) instance))

  (defun-typed leftmost ((tape tape-cons-class-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (make-instance 'cell-cons :cons-cell (cons-list tape))]
      ))

  (defun tape-cons-class-right-neighbor (cell &optional ➜)
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
  ;; accepts a tape-cons-class and a cell, makes the cell the new leftmost
  ;; will be a problem if (cons-list tape) is shared
  (defun-typed epa<cell> ((tape tape-cons-class-active) (cell cell-cons))
    (let(
          (cons-cell (cons-cell cell))
          )
      (rplacd cons-cell (cons-list tape))
      (setf (cons-list tape) cons-cell)
      ))
  (defun-typed epa<cell> ((tape tape-cons-class-empty) (cell cell-cons))
    (let(
          (cons-cell (cons-cell cell))
          )
      (rplacd cons-cell ∅)
      (setf (cons-list tape) cons-cell)
      ))

  ;; accepts a tape-cons-class and an instance, makes a new leftmost initialized with the
  ;; instance will be a problem if (cons-list tape) is shared
  (defun-typed epa<instance> ((tape tape-cons-class-active) instance)
    (let(
          (new-cons-cell (cons instance (cons-list tape)))
          )
    (setf (cons-list tape) new-cons-cell)
    ))
  (defun-typed epa<instance> ((tape tape-cons-class-empty) instance)
    (let(
          (new-cons-cell (cons instance ∅))
          )
      (setf (cons-list tape) new-cons-cell)
      (to-active tape)
      ))

  ;; makes cell-1 a right-neighbor of cell-0
  (defun tape-cons-class-a<cell> (cell-0 cell-1)
    (let*(
           (cons0 (cons-cell cell-0))
           (cons1 (cons-cell cell-1))
           (cons2 (cdr cons0))
           )
      (rplacd cons1 cons2)
      (rplacd cons0 cons1)
      ))

  ;; makes a new right neighbor for cell, and initializes it with instance.
  (defun tape-cons-class-a<instance> (cell-0 instance)
    (let*(
           (cons0 (cons-cell cell-0))
           (cons1 (cons instance (cdr cons0)))
           )
      (rplacd cons0 cons1)
      ))

  ;; removes the leftmost cell and returns it
  ;; will be a problem if (cons-list tape) is shared
  (defun-typed epd<tape> ((tape tape-cons-class-active) &optional ➜)
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

  ;; given a cell removes its right neighbor and returns it
  ;; deletes the right neighbor cell
  (defun tape-cons-class-d<cell> (cell &optional ➜)
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
  (defun-typed ◧d.<tape> ((tape tape-cons-class-active) &optional ➜)
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

  (defun-typed epd*<tape> ((tape tape-cons-class) &optional ➜)
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

  (defun tape-cons-class-d*<cell> (cell &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (rplacd (cons-cell cell) ∅)
      [➜ok]
      ))



