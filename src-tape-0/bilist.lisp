#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Conceptually this is cleaner, but it doesn't bind to Lisp's linked list.  Rather it
creates its own list form.  Perhaps this is what lists would have looked like had CLOS
been part of the original language.

Inheritance structure.

               bilink
                ^  ^
                |   \ 
                |    bilist-with-contents
                |
             list-tape
               ^  ^
               |   |
 list-tape-empty   list-tape-active

The list header will be a list-tape type. The header will hold links to rightmost and
leftmost.

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
  ;; see src-cell/bilist.lisp for bilink type
  (def-type tape-bilist (bilink tape)())
  (def-type tape-bilist-abandoned (tape-bilist)())
  (def-type tape-bilist-valid (tape-bilist tape-valid)())

  (def-type tape-bilist-empty    
    (
      tape-bilist-valid
      tape-empty
      )
    ()
    )
  (def-type tape-bilist-active   
    (
      tape-bilist-valid
      tape-active
      )
    ()
    )

  (defun-typed to-abandoned ((tape tape-bilist)) (change-class tape 'tape-bilist-abandoned))
  (defun-typed to-empty     ((tape tape-bilist)) (change-class tape 'tape-bilist-empty))
  (defun-typed to-active    ((tape tape-bilist)) (change-class tape 'tape-bilist-active))

;;--------------------------------------------------------------------------------
;; init
;;
  (defun-typed init ((tape tape-bilist) (init null) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (cap-off tape)
      (to-empty tape) 
      [➜ok tape]
      ))

;;--------------------------------------------------------------------------------
;; topology queries
;;
  (defun-typed leftmost ((tape tape-bilist-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (right-neighbor-slot tape)] ; blist tape is also a bilink
      ))

  (defun-typed rightmost ((tape tape-bilist-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (left-neighbor-slot tape)]
      ))

;;--------------------------------------------------------------------------------
;; topology manipulation
;;
;;
  (defun-typed epa<tape> ((tape1 tape-bilist-active) (tape0 tape-bilist-active))
    (let(
          (leftmost-tape0  (right-neighbor-slot tape0))
          (rightmost-tape0 (left-neighbor-slot tape0))
          (leftmost-tape1  (right-neighbor-slot tape1))
          )
      ;; prepend tape0
      ;;
        (connect rightmost-tape0 leftmost-tape1)
        (to-interior rightmost-tape0)
        (to-interior leftmost-tape1)

      ;; fix the header
      ;;
        (connect tape1 leftmost-tape0)

      ;; tape0 has been consumed
      ;;
        (cap-off tape0) ; so we don't have any gc problems later
        (to-abandoned tape0)
      ))

  ;; accepts a tape-bilist and a cell, makes the cell the new leftmost
  ;; will be a problem if (cons-bilist tape) is shared
  (defun-typed epa<cell> ((tape tape-bilist-empty) (new-cell cell-bilist))
    (to-active tape)
    (to-solitary new-cell) ; the new cell becomes rightmost
    (insert-between tape tape new-cell)
    )
  (defun-typed epa<cell> ((tape tape-bilist-active) (new-cell cell-bilist))
    (let(
          (c0 tape) ; note the tape header is inherited from bilink, it looks like a tape node
          (c1 (right-neighbor-slot tape))
          )
      (to-interior c1) ; old leftmost is no longer leftmost
      (to-leftmost new-cell) ; the new cell becomes leftmost
      (insert-between c0 c1 new-cell)
      ))

  ;; accepts a tape-bilist and an instance, makes a new leftmost initialized with the
  ;; instance will be a problem if (cons-bilist tape) is shared
  (defun-typed epa<instance> ((tape tape-bilist-valid) instance)
    (let(
          (new-cell (make-instance 'cell-bilist :contents instance))
          )
      (epa<cell> tape new-cell)
      ))

  ;; accepts a tape-bilist and a cell, makes the cell the new rightmost
  ;; will be a problem if (cons-bilist tape) is shared
  (defun-typed ◨a<cell> ((tape tape-bilist-empty) (new-cell cell-bilist))
    (epa<cell> tape new-cell)
    )
  (defun-typed ◨a<cell> ((tape tape-bilist-active) (new-cell cell-bilist))
    (let(
          (c0 (left-neighbor-slot tape))
          (c1 tape) ; note the tape header is inherited from bilink, it looks like a tape node
          )
      (to-interior c0) ; old rightmost is no longer rightmost
      (to-rightmost new-cell) ; the new cell becomes rightmost
      (insert-between c0 c1 new-cell)
      ))

  ;; accepts a tape-bilist and an instance, makes a new rightmost initialized with the
  ;; instance will be a problem if (cons-bilist tape) is shared
  (defun-typed ◨a<instance> ((tape tape-bilist-valid) instance)
    (let(
          (new-cell (make-instance 'cell-bilist :contents instance))
          )
      (◨a<cell> tape new-cell)
      ))


  ;; removes the leftmost cell and returns it
  ;; an active tape always has a leftmost to be deleted
  ;; will be a problem if the tape is intangled, as partners will not have correct leftmost afterward
  (defun-typed epd<tape> ((tape tape-bilist-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let*(
             (c0 tape)
             (c1 (right-neighbor c0)) ; this is leftmost
             (c2 (right-neighbor c1)) ; this is the right neighbor of leftmost, possibly tape
             )
        (when (typep c1 'solitary) (to-empty tape))
        (extract c0 c1 c2)
        [➜ok c1]
        )))

  (defun-typed epd+<tape> ((tape tape-bilist-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let(
            (lm (leftmost tape))
            )
        (cap-off tape)
        (to-empty tape)
        [➜ok lm]
        )))

  ;; deletes rightmost, i.e. ◨-sd
  (defun-typed ep-d<tape> ((tape tape-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let*(
             (c2 tape)
             (c1 (left-neighbor c2)) ; this is rightmost
             (c0 (left-neighbor c1)) ; this is left neighbor of rightmost, possible tape
             )
        (when (typep c1 'solitary) (to-empty tape))
        (extract c0 c1 c2)
        [➜ok c1]
        )))

  (defun-typed d+<tape> ((tape tape-bilist-active) cell &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ()(error 'dealloc-on-rightmost)))
        &allow-other-keys
        )
      ➜
      (d+<cell> cell
        {
          :➜ok (λ(tail)
                 (connect cell tape) ; connect up the lose tape
                 [➜ok tail]
                 )
          :➜rightmost ➜rightmost
          })
      ))
