#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Conceptually this is cleaner, but it doesn't bind to Lisp's linked list.  Rather it
creates its own list form.  Perhaps this is what lists would have looked like had CLOS
been part of the original language.

The list header will be a list-tape type. The header will hold links to right-bound and
left-bound.

Due to list-tape being inherited from link, the list-tape header can be passed to
functions that manipulate links. This simplifies end case processing.

Because list-tape-empty and list-tape-active are inherited from list-tape without the
addiition of slots, we can change-type between them without penalty.  The status of
the list is carried in its type.

Would rather have used static structs, but didn't want to fight Lisp over being able to
change the type of a list-tape-empty to a list-tape-active when list tape is a structure.
Though they are of identical format #'coerce refused to do it. Also, this is a bit
of a research project and I prefer to keep the language syntax paradigm consistent.


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type definition
;;
  ;; tape-list is just a reference to a left-bound cell-list

;;--------------------------------------------------------------------------------
;; primitive topology manipulation
;;
  (defun-typed cap<tape> ((tape cell-list)) 
    (setf (right-neighbor tape) ∅)
    (to-empty tape)
    )

;;--------------------------------------------------------------------------------
;; init
;;
  ;; more advanced tape init found in src-tape-enhancements (when it is written ;-)

  (defun-typed init ((tape tape-list) (init null) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (cap tape)
      [➜ok tape]
      ))

;;--------------------------------------------------------------------------------
;; topology queries
;;
  (defun-typed bound ((tape tape-list-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜bad-direction (λ()(error 'bad-direction)))
        (d *right*)
        &allow-other-keys
        )
      ➜
      (cond
        ((= d *right*) (right-neighbor tape))
        ((= d *left*) (left-neighbor tape))
        (t [➜bad-direction])
        )))

;;--------------------------------------------------------------------------------
;; topology manipulation
;;
;;
  ;; accepts a tape-list and a cell, makes the cell the new left-bound
  ;; will be a problem if (cons-list tape) is shared
  (defun-typed epa<tape> ((tape tape-list-empty) (new-cell cell-list) &optional ➜)
    (to-solitary tape)
    (w tape (r new-cell))
    )
  (defun-typed epa<tape> ((tape tape-list-active) (new-cell cell-list) &optional ➜)
    (destructuring-bind
      (&key
        &allow-other-keys
        )
      ➜
      (let(
            (c1 (right-neighbor tape))
            )
        (if
          (typep c1 'solitary)
          (to-right-bound c1)
          (to-interior c1)
          )
        (connect new-cell c1)
        (connect tape new-cell)
        )))

  ;; removes the left-bound  and returns it, thus making its right-neighbor the new left-bound
  ;; an active tape always has a left-bound to be deleted
  ;; will be a problem if the tape is intangled, as partners will not have correct left-bound afterward
  ;; empty and solitary cases handled on the interface
  (defun-typed epd<tape> ((tape tape-list-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let(
            (c1 (right-neighbor tape))
            )
        (extract tape c1)
        [➜ok c1]
        )))

  ;; empty case handled on the interface
  (defun-typed epd+<tape> ((tape tape-list-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let(
            (left-bound (right-neighbor tape))
            )
        (cap<tape> tape)
        (cap-left left-bound) ; needed for call-next method on bilist types
        [➜ok left-bound]
        )))


