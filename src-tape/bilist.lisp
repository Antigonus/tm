#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Conceptually this is cleaner, but it doesn't bind to Lisp's linked list.  Rather it
creates its own list form.  Perhaps this is what lists would have looked like had CLOS
been part of the original language.  Thus leaving this for later.  Perhaps it will bcome
list2-tape or some such.

.. bringing this back as a doubly linked list


Inheritance structure.

               bilink
                ^  ^
                |   \ 
                |    bilist-with-cargo
                |
             list-tape
               ^  ^
               |   |
 list-tape-empty   list-tape-active


Due to list-tape being inherited from link, the list-tape header can be passed to
functions that manipulate links. This simplifies end case processing.

Because list-tape-empty and list-tape-active are inherited from list-tape without the
addiition of slots, we can change-type between them without penalty.  Status information
about the list is carried in their type.

Would rather have used static structs, but didn't want to fight Lisp over being able to
change the type of a list-tape-empty to a list-tape-active when list tape is a structure.
Though they are of identical format #'coerce refused to do it. Also, this is a bit 
of a research project and I prefer to keep the language syntax paradigm consistent.


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
      (cargo :initarg cargo :accessor cargo)
      ))

  (def-type tape-bilist (bilink tape) ())
  (def-type tape-bilist-empty (tape-bilist))
  (def-type tape-bilist-active (tape-bilist))

  (defun-typed to-empty  ((tape tape-bilist)) (change-class tape 'tape-bilist-empty))
  (defun-typed to-active ((tape tape-bilist)) (change-class tape 'tape-bilist-active))

;;--------------------------------------------------------------------------------
;; initializer
;;
  ;; shallow copies from another tape
  (defun-typed init ((tape-1 tape-bilist) (tape-0 tape) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (setf (right-neighbor tape-1) tape-1)
      (setf (left-neighbor tape-1) tape-1)
      (to-empty tape-1)
      (shallow-copy-topo tape-1 tape-0)
      [➜ok tape-1]
      ))

;;--------------------------------------------------------------------------------
;; topology queries
;;
  (defun-typed =<cell> ((cell-0 cell-bilist) (cell-1 cell-bilist))
    (eq cell-0 cell-1)
    )

  (defun-typed r<cell> ((cell cell-bilist)) (cargo cell))
  (defun-typed w<cell> ((cell cell-bilist) instance) (setf (cargo cell) instance))

  (defun-typed leftmost ((tape tape-bilist-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (right-neighbor tape)]
      ))

  (defun-typed right-neighbor ((cell cell-bilist) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ()(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (let(
            (rn (right-neighbor cell))
            )
        (if (eq rn tape ....


      [➜ok (right-neighbor cell)]
      [➜rightmost]
      ))

  (defun-typed right-neighbor ((cell cell-bilist-rightmost) &optional ➜)
    (destructuring-bind
      (&key
        (➜rightmost (λ()(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  (defun-typed left-neighbor ((cell cell-bilist-rightmost-middle) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (left-neighbor cell)]
      ))

  (defun-typed left-neighbor ((cell cell-bilist-leftmost) &optional ➜)
    (destructuring-bind
      (&key
        (➜leftmost (λ()(error 'step-from-leftmost)))
        &allow-other-keys
        )
      ➜
      [➜leftmost]
      ))
    
;;--------------------------------------------------------------------------------
;; topology manipulation
;;

  ;; for internal use  

  (defun splice (c0 c2 new-cell)
    (setf (left-neighbor new-cell) c0)
    (setf (right-neighbor new-cell) c2)
    (setf (left-neighbor c2) new-cell)
    (setf (right-neighbor c0) new-cell)
    )

  ;; accepts a tape-bilist and a cell, makes the cell the new leftmost
  ;; will be a problem if (cons-bilist tape) is shared
  (defun-typed epa<cell> ((tape tape-bilist-active) (cell cell-bilist))
    (splice tape (right-neighbor tape) cell)
    )

  ;; accepts a tape-bilist and an instance, makes a new leftmost initialized with the
  ;; instance will be a problem if (cons-bilist tape) is shared
  (defun-typed epa<instance> ((tape tape-bilist-empty) instance)
    (let(
          (new-cell (make-instance 'cell-bilist :cargo instance))
          )
      (to-active tape)
      (splice tape (right-neighbor tape) new-cell)
      ))
  (defun-typed epa<instance> ((tape tape-bilist-active) instance)
    (let(
          (new-cell (make-instance 'cell-bilist :cargo instance))
          )
      (splice tape (right-neighbor tape) new-cell)
      ))

  (defun-typed a<cell> ((c0 cell-bilist) (new-cell cell-bilist))
    (splice c0 (right-neighbor c0) new-cell)
    )

  (defun-typed a<instance> ((c0 cell-bilist) instance)
    (let(
          (new-cell (make-instance 'cell-bilist :cargo instance))
          )
      (splice c0 (right-neighbor c0) new-cell)
      ))

--

  ;; removes the leftmost cell and returns it
  ;; will be a problem if (cons-bilist tape) is shared
  (defun-typed epd<tape> ((tape tape-bilist-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let*(
             (leftmost (cons-bilist tape))
             (right-neighbor (cdr leftmost))
             )
        (if right-neighbor
          (setf (cons-bilist tape) right-neighbor)
          (progn
            (setf (cons-bilist tape) ∅)
            (to-empty tape)
            ))
        [➜ok (make-instance 'cell-bilist :cons-cell leftmost)]
        )))

  ;; deletes the right neighbor cell
  (defun-typed d<cell> ((cell cell-bilist) &optional ➜)
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
              [➜ok (make-instance 'cell-bilist :cons-cell cell-1)]
              ))
          [➜rightmost]
          ))))

  ;; References to the right neghbor of leftmost get messed up,
  ;; but (cons-bilist tape) will be ok, so no tape sharing issues
  (defun-typed e-s*d.<tape> ((tape tape-bilist-active) &optional ➜)
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
                  (cell-0 (cons-bilist tape))
                  )
              (setf (cons-bilist tape) ∅)
              (to-empty tape)
              [➜ok (make-instance 'cell-bilist :cons-cell cell-0)]
              ))
          })))

  (defun-typed epd*<tape> ((tape tape-bilist) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (to-empty tape)
      (setf (cons-bilist tape) ∅) ; free the data
      [➜ok]
      ))

  (defun-typed d*<cell> ((cell cell-bilist) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (rplacd (cons-cell cell) ∅)
      [➜ok]
      ))
